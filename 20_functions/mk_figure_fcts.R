

#' Save all figures
#' 
#' This function extracts outcome data (once) and calls `save_std_figure()` to
#' save each figure. 
#'
#' @param outc_var the outcome variable (scalar character)
#' @param set subjects to include
#' @param estimand estimand to use
#' @param tlf_reg optional metadata `data.table` object (cf. `get_metadata()`)
#' @param dt a `data.table` object with data to plot (for histograms) cf. 
#' `get_outcome_data()`.
#' @param verbose Print information on which files are saved to file? 
#' (logical scalar)
#'
#' @returns `NULL` invisibly.
#'
#' @examples
#' 
#' save_all_figures("madrs_total10", verbose = TRUE)
#' 
save_all_figures <- function(outc_var = "madrs_total10", 
                             set      = c("FAS", "SAS"),
                             estimand = c("policy", "efficacy", "safety"), 
                             tlf_reg  = NULL, 
                             dt       = NULL,
                             verbose  = FALSE) {
  # Estimand <- "efficacy"; outc_var <- "madrs_total10"; Set <- "FAS";
  # verbose <- TRUE
  # Get parameters:
  stopifnot(is.character(outc_var) && length(outc_var) == 1L)
  Set      <- match.arg(set)
  Estimand <- match.arg(estimand)
  if(is.null(tlf_reg)) tlf_reg <- get_metadata()
  stopifnot(is.data.table(tlf_reg) && nrow(tlf_reg) > 0L,
            outc_var %in% tlf_reg[, outcome_var])
  
  # Get outcome data:
  if(is.null(dt)) 
    dt <- get_outcome_data(outc_var, tlf_reg, set = Set, estimand = Estimand)
  stopifnot(is.data.table(dt) && nrow(dt) > 0L)
  
  # Make- and save all figures (histograms and chgbl-over-time only):
  eids <- tlf_reg[estimand == Estimand & outcome_var == outc_var & 
                    tlf_type == "figure" & 
                    !grepl("tipping", analysis), 
                  endpoint_id]
  if(length(eids) > 0) for(eid in eids) { # eid <- eids[1]
    save_std_figure(tlf_reg[endpoint_id == eid], 
                    dt, 
                    tlf_reg,
                    verbose = verbose)
  }
  
  invisible(NULL)
}


#' Save figure to png and pdf file
#'
#' @param regi a one-row metadata `data.table` object
#' @param dt a `data.table` object with data to plot (for histograms)
#' @param verbose Print information on which files are saved to file? 
#' (logical scalar)
#'
#' @returns the ggplot figure invisibly
#'
#' @examples
#' 
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[analysis == "chgbl-time"][1]
#' dt <- get_outcome_data("madrs_total10", 
#'                        tlf_reg, 
#'                        set = "FAS", 
#'                        estimand = "policy")
#' save_std_figure(regi, dt, verbose = TRUE)
#' 
#' regi <- tlf_reg[analysis == "histogram"][1]
#' save_std_figure(regi, dt, verbose = TRUE)
#' 
save_std_figure <- function(regi, # save_figure?
                            dt, 
                            tlf_reg = NULL,
                            verbose  = FALSE) {
  stopifnot(is.data.table(regi) && nrow(regi) == 1L,
            is.data.table(dt)   && nrow(dt) > 0,
            is.logical(verbose) && length(verbose) == 1L)
  
  # Extract parameters from regi:
  analysis <- regi[, analysis]
  role     <- regi[, role]
  folder   <- regi[, folder]
  stem     <- regi[, file_stem]

  # Make file paths:
  png_path <- file.path(folder, paste0(stem, ".png"))
  pdf_path <- file.path(folder, paste0(stem, ".pdf"))
  
  # Print 
  if(verbose) message(sprintf("  [save] %s (.png + .pdf)", stem))
  
  # Histograms:
  if(analysis == "histogram") 
    fig <- save_gg_histogram(regi, dt, png_path, pdf_path)
  
  # Change-from-baseline over time:
  if(analysis == "chgbl-time")
    fig <- save_chgbl_time_fig(regi, png_path, pdf_path, tlf_reg = tlf_reg)
  
  if(role == "Subgroup" && analysis == "mmrm")
    fig <- save_subgrp_figure(regi, png_path, pdf_path, tlf_reg = tlf_reg)

  invisible(fig)
}


#' Save histogram
#'
#' @param regi a one-row metadata `data.table` object
#' @param dt a `data.table` object with data to plot
#' @param png_path file name specifying where to save png-figure
#' @param pdf_path file name specifying where to save pdf-figure
#'
#' @returns the ggplot figure invisibly
#'
#' @examples
#' 
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[analysis == "histogram"][1]
#' dt <- get_outcome_data("madrs_total10", 
#'                        tlf_reg, 
#'                        set = "FAS", 
#'                        estimand = "policy")
#' # Make file paths:
#' folder   <- regi[, folder]
#' stem     <- regi[, file_stem]
#' png_path <- file.path(folder, paste0(stem, ".png"))
#' pdf_path <- file.path(folder, paste0(stem, ".pdf"))
#'  
#' # Make figure:
#' fig <- save_gg_histogram(regi,
#'                          dt,
#'                          png_path,
#'                          pdf_path)
#' fig
#' 
save_gg_histogram <- function(regi,
                              dt,
                              png_path,
                              pdf_path) {
  stopifnot(is.data.table(regi) && nrow(regi) == 1L,
            is.data.table(dt)   && nrow(dt) > 0,
            is.character(png_path) && length(png_path) == 1L,
            is.character(pdf_path) && length(pdf_path) == 1L)
  
  # Make histogram(s):
  fig <- mk_gg_histogram(regi = regi, data = dt)
  
  # Height of figure:
  nrows <- dt[, uniqueN(f_visit)] - 1 * (regi[, data_name] == "chgbl")
  hgt <- .3 + 1.5 * nrows
  
  # Save figures:
  ggsave(filename = png_path, plot = fig, width = 7, height = hgt, scale = 1.2, dpi = 300)
  ggsave(filename = pdf_path, plot = fig, width = 7, height = hgt, scale = 1.2)
  
  # Return:
  invisible(fig)
}


#' Save change-from-baseline figure
#'
#' @param regi a one-row metadata `data.table` object
#' @param png_path file name specifying where to save png-figure
#' @param pdf_path file name specifying where to save pdf-figure
#'
#' @returns the ggplot figure invisibly
#'
#' @examples
#' 
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[analysis == "chgbl-time"][1]
#' 
#' # Make file paths:
#' folder   <- regi[, folder]
#' stem     <- regi[, file_stem]
#' png_path <- file.path(folder, paste0(stem, ".png"))
#' pdf_path <- file.path(folder, paste0(stem, ".pdf"))
#' 
#' # Make figure:
#' fig <- save_chgbl_time_fig(regi,
#'                            png_path,
#'                            pdf_path)
#' fig
#' 
save_chgbl_time_fig <- function(regi, 
                                png_path, 
                                pdf_path, 
                                tlf_reg = NULL) {
  # Make figure:
  fig <- mk_chgbl_time_fig(regi, tlf_reg = tlf_reg)
  
  # Save figures:
  ggsave2(filename = png_path, fig, width = 7, height = 5)
  ggsave2(filename = pdf_path, fig, width = 7, height = 5)

  # Return:
  invisible(fig)
}

#' Make Change-from-baseline plot
#'
#' @param regi a one-row metadata `data.table` object.
#'
#' @returns a ggplot object
#'
#' @examples
#' 
#' # Make a change-from-baseline plot for the first occurrence in metadata:
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[analysis == "chgbl-time"][1]
#' regi <- tlf_reg[analysis == "chgbl-time"][20]
#' mk_chgbl_time_fig(regi)
#' 
#' # Now for an efficacy estimand:
#' regi <- tlf_reg[analysis == "chgbl-time" & estimand == "efficacy"][1]
#' mk_chgbl_time_fig(regi)
#' 
mk_chgbl_time_fig <- function(regi, tlf_reg = NULL) { 
  stopifnot(is.data.table(regi) && nrow(regi) == 1L)
  
  ylab    <- regi[, outcome_label]
  em      <- get_plot_data(regi, tlf_reg = tlf_reg) |> get_emmeans_table()
  eps     <- 1 # em[, sd(upr - lwr, na.rm = TRUE)] * 7
  ylimits <- em[, c(floor(min(lwr, na.rm = TRUE) - 3*eps),   # min
                    ceiling(max(upr, na.rm = TRUE) + eps))]  # max
  if(mean(ylimits) > 0) {
    ylimits[1] <- min(0, ylimits[1])
  } else {
    ylimits[2] <- max(0, ylimits[2])
  } 
  xlimits <- c(-.21, 28.5)  # Day 0 to 28

  # Make and combine plots:
  graph_plot <- mk_chgbl_plot(em, 
                              xlimits = xlimits,
                              ylimits = ylimits,
                              xlab    = "Days since Randomization",
                              ylab    = ylab)
  
  at_risk_plot <- mk_at_risk_plot(em, xlimits)
  
  melt_plots(graph_plot, 
             at_risk_plot,
             risk_label = "Available subjects:")
}



#' Melt change-from-baseline and at-risk plots
#'
#' @param graph_plot an object produced by `mk_chgbl_plot()`
#' @param at_risk_plot an object produced by `mk_at_risk_plot()`
#' @param risk_label label for subjects at risk
#'
#' @returns A meltet change-from-baseline and at-risk plot
#' @importFrom cowplot ggdraw align_plots draw_plot draw_label
#'
#' @examples
#' 
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[analysis == "chgbl-time"][1]
#' dttab <- get_plot_data(regi)
#' em <- get_emmeans_table(dttab)
#' p1 <- mk_chgbl_plot(em)
#' p2 <- mk_at_risk_plot(em)
#' 
#' # Melt at-risk and change-from-baseline plots: 
#' melt_plots(p1, p2)
#' 
melt_plots <- function(graph_plot,
                       at_risk_plot,
                       risk_label = "Available subjects:") {
  stopifnot(inherits(graph_plot, "ggplot"),
            inherits(at_risk_plot, "ggplot"),
            is.character(risk_label) && length(risk_label) == 1L)
  
  # Parameters:
  height_prop <- .1
  risk_label_nudge <- .02
  risk_label_size <- 11
  vjust <- 1
  hjust <- -.1
  
  # Combine graph and at-risk plots and return:
  ap <- align_plots(graph_plot, at_risk_plot, align = "v")
  ggdraw() + 
    draw_plot(ap[[2]], height = height_prop, width = 1) + 
    draw_plot(ap[[1]], y = height_prop, height = 1 - height_prop, width = 1) + 
    draw_label(label = risk_label,
               x = risk_label_nudge,
               y = height_prop + risk_label_nudge - .01,
               vjust = vjust, hjust = hjust, size = risk_label_size)
}


#' Make change-from-baseline over time plot
#'
#' @param em a `data.table` with formatted estimated means from an MMRM analysis
#' @param xlimits (numeric length 2) limits of the x-axis
#' @param ylimits (numeric length 2) limits of the y-axis
#' @param xlab (string length 1) label on x-axis
#' @param ylab (string length 1) label on y-axis
#'
#' @returns a change-from-baseline ggplot object
#' 
#' @examples
#' 
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[analysis == "chgbl-time"][1]
#' dttab <- get_plot_data(regi)
#' em <- get_emmeans_table(dttab)
#' mk_chgbl_plot(em)
#' 
#' # Parameters for debugging:
#' # xlimits <- c(-.21, 28.5)
#' # ylimits <- c(-16, 0)
#' # xlab = "Days since Randomization"
#' # ylab = "Mean Change From Baseline"
#' 
mk_chgbl_plot <- function(em, 
                          xlimits = c(-.21, 28.5), 
                          ylimits = c(-16, 0), 
                          xlab = "Days since Randomization",
                          ylab = "Mean Change-from-baseline") {
  stopifnot(is.data.table(em)   && nrow(em) > 0,
            is.numeric(xlimits) && length(xlimits) == 2L,
            is.numeric(ylimits) && length(ylimits) == 2L,
            is.character(xlab)  && length(xlab) == 1L,
            is.character(ylab)  && length(ylab) == 1L)
  # We assume throughout that the last visit is at day 28.
  required_cols <- c("n_visit2", "n_visit", "visit_lab", "emmean", "Treatment", 
                     "lwr", "upr")
  test_required(required_cols, names(em), "Table")
  
  dd <- data.frame(days = 1:7, Ymin = ylimits[1], Ymax = ylimits[2])
  # ylab <- gsub("-", " ", ylab)
  yLab <- parse(text = convert_subscript(ylab)) # to display MADRS~10~ ie. MADRS[10]
  
  trt_levs <- em[, if(is.factor(Treatment)) levels(Treatment) else unique(Treatment)]
  is_fake <- any(grepl("[fF]ake", trt_levs))
  
  label_text <- em[n_visit == 28, .(emmean, Treatment)]
  label_text[, is_min := 1.0 * (abs(min(emmean) - emmean) < 1e-8)]
  lab_text <- data.frame(x = 27, 
                         y = c(label_text[is_min == 1, emmean - 1], 
                               label_text[is_min == 0, emmean + 1.5]),
                         lab = c(label_text[is_min == 1, Treatment],
                                 label_text[is_min == 0, Treatment]))
  xbreaks <- em[, .SD[1, .(visit_lab)], by = n_visit]

  p <- ggplot(em, aes(x = n_visit2, y = emmean)) + 
    theme_classic() + 
    theme(legend.position = "none",
          axis.title.y = element_text(vjust = -18)) + 
    
    # Lines and points:
    geom_line(aes(color = Treatment)) + 
    geom_point(aes(color = Treatment, shape = Treatment)) + 
    
    # X and Y axes:
    scale_x_continuous(xlab, breaks = xbreaks[, n_visit], 
                       labels = xbreaks[, visit_lab],
                       minor_breaks = NULL, limits = xlimits) + 
    scale_y_continuous(yLab, limits = ylimits, 
                       breaks = seq(ylimits[1], ylimits[2], by = 2), 
                       minor = NULL) + 
    
    # Error bars:
    geom_errorbar(mapping = aes(ymin = lwr, ymax = upr, color = Treatment), 
                  width = 0.5) + 
    
    # Treatment labels:
    geom_text(data = lab_text, 
              aes(x = 27, y = y, label = lab, color = lab), 
              size = 3, check_overlap = TRUE, hjust = "right") + 

    # Horizontal line at Y = 0:
    geom_hline(yintercept = 0, linetype = "dashed") + 
    
    # Treatment period:
    geom_ribbon(data=dd, aes(x = days, y = NULL, ymin = Ymin, ymax = Ymax),
                fill="#6F99ADFF", alpha=.2) + 
    geom_text(x = 4, y = ylimits[1] + 2.5, label = "Treatment\nperiod", 
              vjust = 1.4, hjust = "center", size = 3, check_overlap = TRUE) + 
    
    # Color palette:
    scale_color_nejm()
  
  # Add watermark if treatment labels are fake:
  if(is_fake) p <- p + 
    geom_text(x = 14, y = mean(ylimits) + 1, col = "red", 
              label = "Artificial pre-DBL\nTreatment Assignment", 
              size = 11, angle = 45, alpha = .2, check_overlap = TRUE)
  
  p
}


#' Make at-risk plot
#'
#' @param em a `data.table` with formatted estimated means from an MMRM analysis
#' @param xlimits (numeric length 2) limits of the x-axis
#'
#' @returns An at-risk ggplot 
#'
#' @examples
#' 
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[analysis == "chgbl-time"][1]
#' dttab <- get_plot_data(regi)
#' em <- get_emmeans_table(dttab)
#' mk_at_risk_plot(em)
#' 
mk_at_risk_plot <- function(em, 
                            xlimits = c(-.21, 28.5)) {
  # Check arguments:
  stopifnot(is.data.table(em) && nrow(em) > 0L,
            is.numeric(xlimits) && length(xlimits) == 2L)
  required_cols <- c("n_visit", "Treatment", "n")
  test_required(required_cols, names(em), "Table")
  .em <- em[, .SD, .SDcols = required_cols]

  # Make x-axis tic mark placement  
  x_axis_at <- em[grepl("[dD]exa", Treatment), n_visit]
  
  # Make and return plot:
  ggplot(.em, aes(n_visit, y = Treatment, label = n)) + 
    geom_text(hjust = .5, nudge_x = 0, size = 3) + 
    theme_bw() + 
    theme(panel.background = element_rect(fill="white", colour=NA),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          # axis.text = element_blank(),
          axis.text = element_text(colour = "black"),
          axis.title = element_blank(),
          axis.ticks = element_blank()) + 
    scale_x_continuous(breaks = x_axis_at, labels = NULL, limits = xlimits) + 
    labs(x="", y="")
}


#' Get and format table of estimated means
#'
#' @param dttab `data.table` with MMRM analysis results
#'
#' @returns `data.table` with formatted emmeans table.
#'
#' @examples
#' 
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[analysis == "chgbl-time"][1]
#' dttab <- get_plot_data(regi)
#' get_emmeans_table(dttab)
#' 
get_emmeans_table <- function(dttab) {
  stopifnot(is.data.table(dttab))
  set <- getgrep("FAS|SAS", names(dttab))[1L]
  required_cols <- c("f_visit", "Visit", "Treatment", set, "N", "Miss", "emmean", 
                     "lower.CL.emmean", "upper.CL.emmean", "label")
  test_required(required_cols, names(dttab), name = "Analysis Table")
  em <- dttab[, .SD, .SDcols = required_cols]
  
  # Prepare estimated means data:
  setnames(em, c("lower.CL.emmean", "upper.CL.emmean"), c("lwr", "upr"))
  em_day0 <- cbind(
    em[f_visit == f_visit[1], .(Treatment, FAS, N = FAS, Miss = 0)],
    data.table(f_visit = factor("day0"),
               Visit = "Day 0",
               emmean = 0,
               lwr = NA_real_,
               upr = NA_real_,
               label = NA_character_))
  em2 <- rbind(em_day0, em)
  em2[, n_visit := as.numeric(gsub("day", "", f_visit))]
  em2[, visit_lab := as.character(round(n_visit))]
  if(any("MADRS~10~" %in% em2[, label]))
    em2[, visit_lab := fifelse(n_visit == 7, "7*", as.character(round(n_visit)))]
  em2[, n_visit2 := n_visit + fifelse(grepl("[dD]exa", Treatment), .2, -.2)]
  em2[, n := N - Miss]
  
  em2[]
}

#' Get data for plotting change from baseline over time
#'
#' @param regi a one-row metadata `data.table` object.
#' @param tlf_reg 
#'
#' @returns a `data.table` object with analysis results
#'
#' @examples
#' 
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[analysis == "chgbl-time"][1]
#' get_plot_data(regi, tlf_reg = tlf_reg)
#' 
get_plot_data <- function(regi, tlf_reg = NULL) {
  if(is.null(tlf_reg)) tlf_reg <- get_metadata()
  
  # Checks:
  stopifnot(is.data.table(regi) && nrow(regi) == 1L)
  stopifnot(is.data.table(tlf_reg) && nrow(tlf_reg) > 0L)
  outc_kind <- regi[, outcome_kind]
  if(outc_kind != "chgbl")
    stop("outcome_kind: '", outc_kind, "' not allowed. Must be 'chgbl'")
  
  # Endpoints IDs for relevant analysis tables:
  eid_tab <- tlf_reg[outcome_var    == regi[1, outcome_var] &
                       outcome_kind == "chgbl" &
                       role         == "Primary" &
                       estimand     == regi[1, estimand] &
                       tlf_type     == "table",                  
                     .(estimand, visit, endpoint_id, label = variable_label, caption)]
  stopifnot("EIDs are not unique" = 
              eid_tab[, uniqueN(paste(estimand, visit)) == .N])
  
  # Save cross references to 'cache':
  endpoint_id <- regi[1, endpoint_id]
  key         <- eid_tab[, paste0(label, " at Day ", visit)]
  source      <- eid_tab[, endpoint_id]
  add_to_crossref(endpoint_id, key, source)
  
  # Extract data from analysis tables:
  required_cols <- c("f_visit", "Visit", "Treatment", regi[1, subjects], "N", 
                     "Miss", "emmean", 
                     "lower.CL.emmean", "upper.CL.emmean")
  d <- lapply(seq_len(nrow(eid_tab)), function(i) { # i <- 1
    regi <- tlf_reg[endpoint_id == eid_tab[i, endpoint_id]]
    res <- get_dt_table(regi)
    if(!"f_visit" %in% names(res) && "visit" %in% names(res)) 
      res[, f_visit := factor(visit)]
    res <- res[Visit == paste("Day", eid_tab[i, visit]), .SD, .SDcols = required_cols]
    cbind(eid_tab[i, -"caption"], res)
  }) |> rbindlist()
  
  d  
}



#' Add cross references to cache table
#' 
#' Cross references are stored in `02_data_derived/crossrefs.csv`
#'
#' @param endpoint_id 
#' @param key 
#' @param source 
#' @param replace 
#'
#' @returns `NULL` invisibly. Updates `.csv` file with cache of cross refs.
#'
#' @examples
#' 
#' # parameters for debugging:
#' # endpoint_id <- "E23-07"
#' # key <- eid_tab[, paste0(label, " at Day ", visit)]
#' # source <- eid_tab[, endpoint_id]
#' # add_to_crossref(endpoint_id = "E23-07", key, source)
#' # add_to_crossref(endpoint_id = "E23-08", key, source)
#' 
add_to_crossref <- function(endpoint_id, key, source, replace = TRUE) {
  stopifnot(is.character(endpoint_id) && length(endpoint_id) == 1L,
            is.character(key) && length(key) >= 1L,
            is.character(source) && length(source) >= 1L,
            length(key) == length(source),
            is.logical(replace) && length(replace) == 1L &&
              replace %in% c(TRUE, FALSE))
  
  File = "02_data_derived/crossrefs.csv"
  has_file <- file.exists(File)
  eid <- endpoint_id
  
  # cross-refs to add:
  add_cr <- data.frame("endpoint_id" = endpoint_id,
                       "key" = key, 
                       "source" = source) |> as.data.table()

  # Make or modify csv-file with cross references:
  if(!file.exists(File)) { # Make the file if it doesn't exist
    fwrite(add_cr, file = File, sep = ";")
    return(invisible(NULL))
  }
  old_cr <- fread(File)
  if(has_file && replace) { # update entries if replace = TRUE
    new_cr <- rbind(old_cr[endpoint_id != eid], add_cr)
  }
  if(file.exists(File) && !replace) {
    old_cr_eid <- old_cr[, endpoint_id]
    new_cr <- rbind(old_cr, add_cr[!endpoint_id %in% old_cr_eid])
  }
  fwrite(new_cr, file = File, sep = ";")
  
  invisible(NULL)
}
# get_crossref_line("E23-07")
# get_crossref_line("E23-08")


#' Get footer line for cross references
#'
#' @param endpoint_id 
#'
#' @returns
#' @export
#'
#' @examples
#' 
#' get_crossref_line("E23-07")
#' get_crossref_line("E23-08")
#' 
get_crossref_line <- function(endpoint_id) {
  stopifnot(is.character(endpoint_id) && length(endpoint_id) == 1L)
  eid <- endpoint_id
  
  # Get list of cross references:    
  cr_tab <- fread("02_data_derived/crossrefs.csv")
  if(!eid %in% cr_tab[, endpoint_id]) return(NULL)
  
  # Make footer line with cross references:
  lines <- cr_tab[endpoint_id == eid, paste0(key,": [-@sec-", source, "]")]
  line1 <- paste0(
    "Presentation is based on the statistical analysis table", 
    if(length(lines) > 1) "s: " else ": ")
  
  paste0(line1, paste(lines, collapse = ", "))
}


#' Save subgroup figure to file
#'
#' @param regi 
#' @param png_path 
#' @param pdf_path 
#' @param tlf_reg 
#'
#' @returns
#'
#' @examples
#' 
#' # tlf_reg <- get_metadata()
#' # regi <- tlf_reg[tlf_type == "figure" & role == "Subgroup"][1]
#' 
save_subgrp_figure <- function(regi, 
                               png_path, 
                               pdf_path, 
                               tlf_reg = NULL) {
  # Make figure:
  fig <- mk_subgrp_figure(regi, tlf_reg = tlf_reg)
  
  # Save figures:
  p_wh <- get_wh(fig)
  ggsave2(filename = png_path, plot=fig, width=p_wh["width"], height = p_wh["height"])
  ggsave2(filename = pdf_path, plot=fig, width=p_wh["width"], height = p_wh["height"])
  
  # Return:
  invisible(fig)
}


#' Make subgroup figure
#'
#' @param regi 
#' @param tlf_reg 
#'
#' @returns
#' @export
#'
#' @examples
#' 
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[tlf_type == "figure" & role == "Subgroup"][1]
#' mk_subgrp_figure(regi, tlf_reg = tlf_reg)
#' 
mk_subgrp_figure <- function(regi, tlf_reg = NULL) { 
  stopifnot(is.data.table(regi) && nrow(regi) == 1L)
  
  # Get data for subgroup figure:
  .d <- get_subgrp_fig_data(regi, tlf_reg = tlf_reg)
  subtab <- .d[, .SD, .SDcols = attr(.d, "use_cols")]
  setnames(subtab, "plot", " ")
  
  # Set background to white and summary diamond to black
  tm <- forest_theme(core = list(bg_params = list(fill = c("white"))),
                     vertline_lty = "dotted")

  # Get lower x-axis limit:
  ci_min <- .d[, min(lower.CL, na.rm = TRUE)]
  xmin <- pmin(0, 5 * floor(ci_min / 5))

  # Make forest plot using forestploter::forest:
  p <- forest(data = subtab,
              theme = tm,
              est = .d[, estimate],
              lower = .d[, lower.CL],
              upper = .d[, upper.CL],
              ref_line = .d[Subgroup == "All subjects", estimate],
              xlim = c(xmin, 5), vert_line = 0, 
              # ticks_at = c(-10, -5, 0),
              # ticks_minor = 2,
              ci_column = 4)
  
  # Adjust appearance of plot:
  p <- insert_text(p, text = "FAS",
                   part = "header", 
                   row = 1,
                   col = 2:3,
                   gp = gpar(fontface = "bold"))
  p <- add_border(p, # h-line over Dexa Plac no. subjects
                  part = "header", 
                  row = 1,
                  col = 2:3,
                  gp = gpar(lwd = .5))
  p <- add_border(p, # hline below header
                  part = "header", 
                  row=2,
                  gp = gpar(lwd = .5))
  p <- edit_plot(p, # right-align p-value column
                 col = 6, 
                 which = "text", 
                 hjust = unit(1, "npc"), 
                 x = unit(0.9, "npc"))
  
  # Return the plot:
  p
}


#' Get data for subgroup figure
#'
#' @param regi 
#' @param tlf_reg 
#'
#' @returns
#' @examples
#' 
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[tlf_type == "figure" & role == "Subgroup"][1]
#' get_subgrp_fig_data(regi, tlf_reg = tlf_reg)
#' 
get_subgrp_fig_data <- function(regi, tlf_reg = NULL) {
  stopifnot(is.data.table(regi) && nrow(regi) == 1L)
  if(is.null(tlf_reg)) tlf_reg <- get_metadata()
  stopifnot(is.data.table(tlf_reg) && nrow(tlf_reg) > 0L)
  
  # Get values from 'regi':  
  Estimand  <- regi[, estimand]
  nvisit    <- regi[, visit]
  Analysis  <- regi[, analysis]
  outc_var  <- regi[, outcome_var]
  outc_kind <- regi[, outcome_kind]
  
  # Get data for "all-subjects" row:
  regi_main <- tlf_reg[estimand == Estimand & 
                         outcome_var == outc_var & 
                         outcome_kind == outc_kind & 
                         visit == nvisit &
                         analysis == Analysis & 
                         role != "Subgroup"]
  
  stopifnot(nrow(regi_main) == 1L)
  main_eid <- regi_main[, endpoint_id]
  mainfit <- get_dt_table(regi_main)
  part1 <- mainfit[f_visit == "day7", .(f_visit, trt, FAS)]
  part2 <- mainfit[f_visit == "day7" & !is.na(estimate), 
                   .(f_visit, estimate, lower.CL, upper.CL, `Effect (95% CI)`)]
  main <- merge(dcast(part1, f_visit ~ trt, value.var = "FAS"), 
                part2, by="f_visit")
  main <- cbind("Subgroup" = "All subjects", main[, -"f_visit"])
  
  # Get data on subgroups:  
  regis <- tlf_reg[estimand == Estimand & 
                     outcome_var == outc_var & 
                     outcome_kind == outc_kind & 
                     visit == nvisit &
                     analysis == Analysis & 
                     tlf_type == "table" & 
                     role == "Subgroup"]
  a <- lapply(seq_len(regis[, .N]), function(i) { # i <- 1
    Subgrp_label <- regis[i, subgrp_label]
    tab <- get_dt_table(regis[i])
    tab[, subgrp_label := Subgrp_label]
    tab[, "subgrp_level" := get(Subgrp_label)]
    tab[, .SD, .SDcols = setdiff(names(tab), Subgrp_label)]
  }) |> rbindlist()
  b <- a[f_visit == paste0("day", nvisit)]
  trt_levs <- get_trt_levs(b)
  
  sgrp_labs <- b[, unique(subgrp_label)]
  subtab0 <- lapply(sgrp_labs, function(slab) { # slab <- "Sex"
    .d <- b[subgrp_label == slab]
    part1 <- .d[, .(subgrp_level, trt, FAS)]
    part2 <- .d[!is.na(estimate), .(
      subgrp_level, estimate, lower.CL, upper.CL, `Effect (95% CI)`, `p-het`, het.pvalue)]
    subtab2 <- merge(dcast(part1, subgrp_level ~ trt, value.var = "FAS"),
                     part2, by="subgrp_level", sort=FALSE)
    subtab1 <- subtab2[1]
    subtab1[] <- NA
    subtab1[, subgrp_level := slab]
    subtab <- rbind(subtab1, subtab2)
    setnames(subtab, "subgrp_level", "Subgroup")
    subtab[]
  }) |> rbindlist()
  
  # Merge data on 'all subjects' and subgroups:
  subtab <- rbind(main, subtab0, fill=TRUE)
  stopifnot(all(trt_levs %in% names(subtab)))
  ## Format table:
  subtab[!is.na(get(trt_levs[1])) & Subgroup != "All subjects", 
         Subgroup := paste0("    ", Subgroup)]
  subtab[, names(.SD) := lapply(.SD, function(x) fifelse(is.na(x), "", as.character(x))), 
         .SDcols = c(trt_levs, "Effect (95% CI)", "p-het")]
  setnames(subtab, "p-het", "p-value")
  subtab[, plot := paste(rep(" ", 20), collapse = " ")]
  
  # Add cross references to cache:  
  keys <- c("All subjects", regis[, subgrp_label])
  sources <- c(regi_main[, endpoint_id], regis[, endpoint_id])
  add_to_crossref(endpoint_id = regi[, endpoint_id],
                  key         = keys,
                  source      = sources)
  
  # Prepare result value:
  use_cols <- c("Subgroup", trt_levs, "plot", "Effect (95% CI)", "p-value")
  setattr(subtab, "use_cols", use_cols)
  subtab[]
}


#' Make ggplot histogram
#'
#' @param regi a one-row metadata `data.table` object
#' @param dt a `data.table` object with data to plot
#'
#' @returns
#' @examples
#' 
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[analysis == "histogram"][1]
#' dt <- get_outcome_data("madrs_total10",
#'                        tlf_reg,
#'                        set = "FAS",
#'                        estimand = "policy")
#' 
#' # Make figure:
#' mk_gg_histogram(regi, dt)
#' 
#' # Histogram for censored data:
#' dt <- get_outcome_data("crp_crp",
#'                        tlf_reg,
#'                        set = "FAS",
#'                        estimand = "policy")
#' regi <- tlf_reg[outcome_var == "crp_crp" & tlf_type == "figure"]
#' 
#' # Make figure:
#' mk_gg_histogram(regi, dt)
#' 
mk_gg_histogram <- function(regi, data) {
  # Check arguments 
  stopifnot(is.data.table(data) && nrow(data) > 1,
            is.data.table(regi) && nrow(regi) == 1)
  required_cols <- c("value", "f_visit", "trt")
  test_required(required_cols, names(data))
  data_name <- regi[, data_name]
  .d <- copy(data)
  if(data_name == "chgbl") .d <- .d[f_visit != "day0"] |> droplevels()
  
  # Parameters:  
  trans <- regi[, transformation]
  xlab  <- regi[, outcome_label]
  if(grepl("cens", trans)) {
    .d <- add_cens_value(.d)
    .d <- .d[!is.na(cens_lloq) & cens_lloq == ">LLOQ"]
  }
  .d[, .value := get(data_name)]
  if(grepl("log", trans)) {
    xlab <- paste0("log~10~(", xlab, ")")
    .d <- .d[.value > 0] |> droplevels()
  }
  .d <- .d[!is.na(.value)] |> droplevels()
  
  # Make visit labels:
  .d[, Visit := f_visit]
  levs <- levels(.d[, f_visit])
  levs <- gsub("day", "Day ", levs)
  levs <- gsub("month", "Month ", levs)
  levels(.d$Visit) <- levs
  
  # Make plot:  
  fig <- ggplot(.d, aes(.value)) + geom_histogram(bins = 20) + 
    theme_bw() + theme(axis.title.x = ggtext::element_markdown()) + 
    scale_y_continuous("Count") + 
    facet_grid(Visit ~ trt)
  fig <- if(grepl("log", trans)) fig + scale_x_log10(xlab, minor_breaks = NULL) else 
    fig + scale_x_continuous(xlab, minor_breaks = NULL)
  
  fig  
}


#' Convert sub-script in markdown to plotmath
#' 
#' Convert a string such as `"HAM-D~10~ Change-from-baseline"` to 
#' "`HAM-D`[10]~`Change-from-baseline`" where strings are backtick'ed 
#' (to preserve dashes) and "~10~" is converted to "[10]". Note that if strings
#' with dashes are not backtick'ed dashes are interepreted as 'minus' and 
#' displayed as such.
#' 
#' The motivation for this function:
#' 
#' The ggtext-package provides the ggplot-compatible `element_markdown()` 
#' function but it appears to not respect the `element_text()` argument `vjust`.
#' We therefore convert the axis labels in Markdown to using 
#' `convert_subscript()` and the native `element_text(vjust = -10)`. 
#'
#' @param x a string with potential markdown notation for sub-script and/or 
#' dashes that needs to be preserved as such. 
#'
#' @returns a string in plotmath notation
#'
#' @examples
#' 
#' # Example of conversion.
#' ylab <- "HAM-D~10~ Change-from-baseline"
#' convert_subscript(ylab)
#' 
#' # For plotting the label has to be parsed:
#' (ylab <- parse(text = convert_subscript(ylab)))
#' 
#' # Example plot in conjunction with element_text:
#' tmp <- data.table(x = 1:10, y = rnorm(1:10))
#' ggplot(tmp, aes(x, y)) + geom_point() +
#'   theme(axis.title.y = element_text(vjust = -15)) +
#'   scale_y_continuous(ylab)
#' 
#' # If strings are not backtick'ed any dash is interpreted as a minus sign:
#' (ylab2 <- parse(text = "MADRS[10]~Change-from-baseline"))
#' ggplot(tmp, aes(x, y)) + geom_point() +
#'   scale_y_continuous(ylab2)
#'   
convert_subscript <- function(x) {
  x <- gsub("~([^~]+)~", "[\\1]", x)
  x <- gsub("([^\\s\\[\\]]*-[^\\s\\[\\]]*)", "`\\1`", x, perl = TRUE)
  x <- gsub(" ", "~", x)
  x
}


#' Make retention/persistence over time plot
#'
#' @param dt a `data.table with retention or persistence data
#' @param xlimits (numeric length 2) limits of the x-axis
#' @param ylimits (numeric length 2) limits of the y-axis
#' @param legend_pos (numeric length 2) relative placement of treatment 
#' legend labels on the y-axis
#' @param xlab (string length 1) label on x-axis
#' @param ylab (string length 1) label on y-axis
#'
#' @returns a retention/persistence ggplot object
#' 
#' @examples
#' 
#' dt <- readRDS("02_data_derived/patient_retention.rds")
#' mk_retention_plot(dt)
#' 
#' # Parameters for debugging:
#' # xlimits <- c(-.21, 28.5)
#' # ylimits <- c(0, 101)
#' # legend_pos <- c(-1,1.5)
#' # xlab = "Days since Randomization"
#' # ylab = "Patient retained in study (%)"
#' 
mk_retention_plot <- function(dt,
                             xlimits = c(0, 28.5), 
                             ylimits = c(0, 101), 
                             xlab = "Days since randomization",
                             ylab = "Patients retained in study (%)",
                             legend_pos = c(-1, 1.5)
                             ){
  # Check arguments
  stopifnot(is.data.table(dt)   && nrow(dt) > 0,
            is.numeric(xlimits) && length(xlimits) == 2L,
            is.numeric(ylimits) && length(ylimits) == 2L,
            is.numeric(legend_pos) && length(legend_pos) == 2L,
            is.character(xlab)  && length(xlab) == 1L,
            is.character(ylab)  && length(ylab) == 1L)
  # Check columns
  required_cols <- c("visit_lab", "n_target", "pct", "trt")
  test_required(required_cols, names(dt), "Table")
  

  # Data used to add treatment period on step plot 
  dd <- data.table(days = 1:7, Ymin = ylimits[1], Ymax = ylimits[2])
  
  # Breaks and labels for x-axis in step plot
  xbreaks <- dt[, .SD[1, .(visit_lab)], by = n_target]
  
  # Text and position for treatment labels in step plot
  stopifnot(dt[, any(c(28, 5) %in% n_target)])
  lab_x_pos <- dt[, if (28 %in% n_target) 28 else 5]
  label_text <- dt[n_target == lab_x_pos, .(pct, trt)]
  label_text[, is_min := 1.0 * (abs(min(pct) - pct) < 1e-8)]
  legend_text <- data.frame(x = lab_x_pos - lab_x_pos/28, 
                            y = c(label_text[is_min == 1, pct + legend_pos[1]],
                                  label_text[is_min == 0, pct + legend_pos[2]]),
                            lab = c(label_text[is_min == 1, trt],
                                    label_text[is_min == 0, trt]))
  
  # Initiate step plot
  step <- ggplot(dt, aes(x = n_target, y = pct)) + 
    theme_classic() + 
    theme(legend.position = "none",
          axis.title.y = element_text(vjust = - 1)) + 
    
    # Lines and points:
    geom_step(aes(color = trt, linetype = trt)) + 
    geom_point(aes(color = trt, shape = trt), stroke = 0.8) +
    scale_shape_manual(values = c(16, 4)) +
    scale_linetype_manual(values = c("solid", "twodash")) +
    
    # X and Y axes:
    scale_x_continuous(xlab, breaks = xbreaks[, n_target], 
                       labels = xbreaks[, visit_lab],
                       minor_breaks = NULL, limits = xlimits) + 
    scale_y_continuous(ylab, limits = ylimits, 
                       breaks = pretty(c(ylimits[1], 100), n = 4), 
                       minor = NULL,
                       expand = c(0,0)) + 
    # Treatment legend
    geom_text(data = legend_text, 
              aes(x = x, y = y, label = lab, color = lab), 
              size = 3, check_overlap = TRUE, hjust = "right") + 
    
    # Treatment period:
    geom_ribbon(data=dd, aes(x = days, y = NULL, ymin = Ymin, ymax = Ymax),
                fill="#6F99ADFF", alpha=.2) + 
    geom_text(x = 4, y = ylimits[1] + (ylimits[2]-ylimits[1]) / 7, label = if (xlimits[2] > 10) "Treatment\nperiod" else "Treatment period", 
              vjust = 1.4, hjust = "center", size = 3, check_overlap = TRUE) + 
    
    # Color palette:
    scale_color_nejm()
  
  # Indicator of treatment variable is fake
  trt_levs <- dt[, if(is.factor(trt)) levels(trt) else unique(trt)]
  is_fake <- any(grepl("[fF]ake", trt_levs))
  
  # Add watermark if treatment labels are fake:
  if(is_fake) step <- step + 
    geom_text(x = 14, y = mean(ylimits) + 1, col = "red", 
              label = "Artificial pre-DBL\nTreatment Assignment", 
              size = 11, angle = 45, alpha = .2, check_overlap = TRUE)
  step
}

scale_color_custom <- function(pal, palette, n, order, alpha = 1) {
  pal <- getFromNamespace(paste0("pal_", pal), "ggsci")
  
  colors <- if (missing(palette)) {
    pal(alpha = alpha)(n)
  } else {
    pal(palette = palette, alpha = alpha)(n)
  }
  
  if (length(order) > length(colors)) {
    stop("The length of order exceeds the number of colors.", call. = FALSE)
  }
  colors <- if (!missing(order)) colors[order]
  
  ggplot2::scale_color_manual(values = colors)
}

