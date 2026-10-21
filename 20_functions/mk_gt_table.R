

#' Save all `gt()` tables for an outcome-variable
#'
#' @param outc_var outcome variable (a string)
#' @param estimand estimand
#' @param tlf_reg optional metadata object (result of `get_metadata()`)
#' @param dttabs optional list of analysis results tables each being a
#' `data.table` object
#' @param verbose print information on saved tables? (logical scalar)
#'
#' @returns `NULL` invisibly
#'
#' @examples
#' 
#' save_all_gt_tables("mdi_total")
#' 
save_all_gt_tables <- function(outc_var = "madrs_total10", 
                               estimand = c("policy", "efficacy", "safety"),
                               tlf_reg  = NULL,
                               dttabs   = NULL, 
                               verbose  = TRUE) {
  # outc_var = "madrs_total10"; Estimand <- "policy"; tlf_reg <- NULL;
  # Check arguments:
  Estimand <- match.arg(estimand)
  if(is.null(tlf_reg)) tlf_reg  <- get_metadata()
  stopifnot(is.character(outc_var) && length(outc_var) == 1L,
            is.data.table(tlf_reg) && nrow(tlf_reg) > 0L,
            outc_var %in% tlf_reg[, outcome_var],
            is.logical(verbose) && length(verbose) == 1L)
  if(is.null(dttabs)) {
    regi <- tlf_reg[outcome_var == outc_var & 
                      estimand == Estimand & 
                      tlf_type == "table"][1]
    dttabs <- get_dt_table(regi, return = "list")
  }
  
  # Parameters:
  table_footers <- get_footers()
  regi_all      <- tlf_reg[outcome_var == outc_var & estimand == Estimand]
  eids          <- regi_all[, endpoint_id]
  
  ## Now run all non-null data tables:
  ii <- setNames(seq_along(eids), eids)
  for(i in ii) { # i <- 1
    eid <- eids[i]
    dttab <- dttabs[[eid]]
    if(is.null(dttab) || nrow(dttab) == 0L) next
    regi <- regi_all[endpoint_id == eid]
    dp_dttab <- display_dttab(dttab)
    footer_list <- mk_footer_list(regi, dp_dttab, table_footers)
    dp_dttab |> 
      mk_gt_table(footer_list = footer_list) |>
      save_gt_table(regi=regi, verbose = verbose)
  }
  
  invisible(NULL)
}


#' Save `gt()` table as html and tex to disk
#'
#' @param gt_obj a `gt()` table object
#' @param regi a one-row metadata `data.table`
#' @param verbose print information on save process? (logical scalar)
#'
#' @returns the html and tex paths invisibly
#'
#' @examples
#' 
#' # Get statistical results:
#' tlf_reg <- get_metadata()
#' outc_var <- "mdi_total"
#' regi <- tlf_reg[outcome_var == outc_var & tlf_type == "table"][1L]
#' dttab <- get_dt_table(regi)
#' 
#' # Make a gt-table object with footers:
#' footers <- mk_footer_list(regi, dttab)
#' gt_obj <- mk_gt_table(dttab, footers)
#' 
#' # Save gt table:
#' save_gt_table(gt_obj, regi, verbose = TRUE)
#' 
save_gt_table <- function(gt_obj, 
                          regi, 
                          verbose    = FALSE) {
  # Check arguments:
  stopifnot(inherits(gt_obj, "gt_tbl"),
            is.data.table(regi) && nrow(regi) == 1L,
            is.logical(verbose) && length(verbose) == 1L)
  
  # Get file stem and folder from 'regi':
  stem       <- regi[, file_stem]
  tables_dir <- regi[, folder]
  
  if(verbose) message(sprintf("  [save] %s (.html + .tex)", stem))
  
  # Write tables to files:
  html_path <- file.path(tables_dir, paste0(stem, ".html"))
  tex_path  <- file.path(tables_dir, paste0(stem, ".tex"))
  writeLines(as_raw_html(gt_obj),                 html_path)
  writeLines(as.character(as_latex(gt_obj)),       tex_path)
  
  invisible(c(html = html_path, tex = tex_path))
}



#' Make a `gt()` table from a `data.table` of statistical results
#'
#' @param dttab `data.table` of statistical results
#' @param footer_list an optional list of footers (strings) to set on the 
#' `gt()` table
#'
#' @returns a `gt()` table of statistical results optionally with footers
#'
#' @examples
#' 
#' # Get statistical results:
#' tlf_reg <- get_metadata()
#' outc_var <- "mdi_total"
#' regi <- tlf_reg[outcome_var == outc_var & tlf_type == "table"][1L]
#' dttab <- get_dt_table(regi)
#' 
#' # Make a gt-table object:
#' mk_gt_table(dttab)
#' 
#' # Setting footers too:
#' footers <- mk_footer_list(regi, dttab)
#' mk_gt_table(dttab, footers)
#' 
mk_gt_table <- function(dttab,
                        footer_list = NULL) {
  stopifnot(is.data.table(dttab))
  
  .d <- copy(dttab)
  ddttab <- display_dttab(.d)
  
  # Make gt object with set style:
  gt_obj <- gt(ddttab) |>
    sub_missing(columns = everything(), missing_text = "") |>
    # apply_col_props(props) |> # col widths
    set_gt_style() 

  # Indent under "Any treatment" if present in "Treatment" column: 
  if("Treatment" %in% names(.d) & any(grepl("[Aa]ny", .d[, Treatment]))) {
    .d[, grepl("[Aa]ny", Treatment)]
    .d[, indent_level := 1.0 * !grepl("[Aa]ny", Treatment)]
    
    # Indent rows with indent_level == 1
    if(.d[, uniqueN(indent_level) > 1]) { # only indent if we also don't
      gt_obj <- tab_style(
        gt_obj,
        style = cell_text(indent = px(20)),
        locations = cells_body(
          columns = "Treatment",
          rows = .d$indent_level == 1
        )
      )
    }
  }
  
  # Add footnotes as source notes, rendered as markdown:
  if(!is.null(footer_list)) for(footer in footer_list) {
    gt_obj <- gt_obj |>
      tab_source_note(source_note = md(as.character(footer)))
  }
  
  # Return:
  gt_obj
}


#' Set style on `gt()` objects
#' 
#' @description
#' Alignment:
#'   All columns right-aligned, except Visit and trt which are left-aligned.
#' 
#' Other:
#' - White background, no row striping.
#' - 2px top/bottom outer rules; color only overridden (gt defaults to solid
#'     2px, so style and width settings are not needed).
#' - Group header rows (Day X) match data row size and padding, with a
#'     thin top rule as the only group separator.
#'
#' @param gt_obj a `gt()` table object
#'
#' @returns the `gt()` table object with styles set
#'
#' @examples
#' 
#' # Get statistical results:
#' tlf_reg <- get_metadata()
#' outc_var <- "mdi_total"
#' regi <- tlf_reg[outcome_var == outc_var & tlf_type == "table"][1L]
#' dttab <- get_dt_table(regi)
#' 
#' # Make a gt-table object:
#' gt_obj <- dttab |> display_dttab() |> gt()
#' 
#' # Set gt style:
#' set_gt_style(gt_obj)
#' 
set_gt_style <- function(gt_obj) {
  stopifnot(inherits(gt_obj, "gt_tbl"))
  
  # Alignment: right everywhere, then left for label columns:
  left_cols <- c("visit", "Visit", "trt", "Treatment", "treatment")
  gt_obj <- gt_obj |>
    cols_align(align = "right", columns = everything()) |>
    cols_align(align = "left",  columns = any_of(left_cols))
  
  # Table-level options:
  gt_obj <- gt_obj |>
    tab_options(
      table.width                       = pct(100),
      table.font.size                   = px(12),
      data_row.padding                  = px(3),
      latex.tbl.pos                     = "H",
      table.border.top.color            = "#333333",
      table.border.bottom.color         = "#333333",
      table_body.border.bottom.color    = "#333333",
      table_body.border.bottom.width    = px(1),
      table_body.border.top.color       = "#333333",
      table_body.border.top.width       = px(1),
      table_body.hlines.style           = "none",
      # table_body.border.top.style       = "none",
      column_labels.font.weight         = "bold",
      column_labels.border.top.style    = "none",   # Removes duplicate rule above header.
      column_labels.border.bottom.style = "none",   
      # column_labels.border.bottom.width = px(1),
      # column_labels.border.bottom.color = "#333333",
      # row_group.background.color        = "#FFFFFF",
      # row_group.font.weight             = "bold",
      # row_group.font.size               = px(12),
      # row_group.padding                 = px(3),
      # # row_group.border.top.color        = "#D3D3D3",
      # row_group.border.top.style        = "none",
      # row_group.border.bottom.style     = "none",
      row.striping.include_table_body   = FALSE
    )
  
  gt_obj
}

