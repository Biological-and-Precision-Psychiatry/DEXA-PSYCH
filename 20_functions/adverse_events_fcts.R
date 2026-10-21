
#' Summarize individuals in the Safety Analysis Set
#'
#' @description
#' Generates either a long-format count table or a single-row wide-format
#' `data.table` suitable for use as a header row in gt summary tables,
#' depending on the `wide_format` argument, summarizing the number of individuals
#' in the Safety Analysis Set (SAS).
#'
#' @param sas_data `data.table` with information on safety analysis status.
#'  Required columns `c("ID", "c_SAS")`. 
#' @param wide_format Logical. If `FALSE` (default), returns a long-format
#'  `data.table` with columns `c("trt", "N_total")` containing the number of
#'  unique individuals in SAS per treatment group and overall. If `TRUE`,
#'  returns a single-row wide-format `data.table` with columns
#'  `c("high_level_text", "text", "indent_level", "N_<trt>", "pct_<trt>",
#'  "E_<trt>")` where `<trt>` refers to each treatment group and `Total`.
#'
#' @return A `data.table`. See `wide_format` for details on the two possible
#'   formats.
#' 
#' @example 
#' 
#' # Summary of individuals in SAS (wide format)
#' mk_sas_summary(wide_format = TRUE)
#' 
mk_sas_summary <- function(
    sas_data    = readRDS("02_data_derived/SAS_population.rds"),
    wide_format = FALSE) 
{
  test_required(c("ID", "c_SAS"), names(sas_data))
  sas_data <- add_derived_data(sas_data, which = c("trt"))
  
  # Number of individuals in SAS:
  N_trt <- rbind(sas_data[c_SAS == "yes", .(N_total = uniqueN(ID)), by = trt],
                 data.table(trt = "Total", N_total = sas_data[c_SAS == "yes", uniqueN(ID)]))
  
  if (wide_format == TRUE) { # Make SAS row in wide format
    sas_row <- data.table(
      high_level_text = "Safety analysis set",
      text            = "Safety analysis set",
      indent_level    = 0L
    )
    for (t in N_trt$trt) {
      sas_row[, paste0("N_",   t) := N_trt[trt %in% t, N_total]]
      sas_row[, paste0("pct_", t) := "100%"]
      sas_row[, paste0("E_",   t) := ""]
    }
    sas_row
  } else {
    N_trt
  }
}

#' Create summary row for any adverse event
#'
#' @description
#' Generates a `data.table` in wide format summarizing counts, percentages,
#' and event numbers for all adverse events by treatment group and total.
#' The output is formatted for direct use with `mk_ae_gt()`.
#'
#' @param event_data `data.table` containing individual-level adverse event 
#' data (one row per adverse event). Must include columns `c("ID", "trt")` 
#' 
#' @return A `data.table` with columns `c("high_level_text", "text",
#'   "indent_level", "N_<trt>", "pct_<trt>", "E_<trt>")` where `<trt>`
#'   refers to each treatment group and total.
#'   
#' @example 
#' 
#' # Get event data
#' de <- readRDS("02_data_derived/adverse_events_derived.rds")
#' 
#' # Summarize all events
#' mk_any_ae_row(de)
#' 
mk_any_ae_row <- function(event_data){
  test_required(c("ID", "trt"), names(event_data))
  # Number of individuals in SAS
  N_trt <- mk_sas_summary()
  
  # Number of individuals and number of adverse events
  result_trt <- event_data[, .(N = uniqueN(ID), E = .N), by = c("trt")][
    N_trt, pct := 100 * (N / N_total), on = "trt"]
  result_total <- event_data[, .(trt = "Total", N = uniqueN(ID), E = .N)][
    N_trt, pct := 100 * (N / N_total), on = "trt"]
  result <- rbind(result_trt, result_total)
  # Indicator of indent for gt table
  result[, indent_level := 0]
  
  # Variables needed for gt table
  result[, text := "Any adverse event"]
  result[, high_level_text := "Any adverse event"]
  
  # Pivot to wide format
  result_wide <- dcast(result, high_level_text + text + indent_level ~ trt, 
                       value.var = c("pct", "N", "E"), fill = 0, drop = FALSE)
  
  # Format percent variable
  n_pct_cols <- grep("^pct", names(result_wide), value = TRUE)
  result_wide[, (n_pct_cols) := lapply(.SD, function(x) paste0(do_format(x, 1), "%")), 
              .SDcols = n_pct_cols]

  result_wide[]
}

#' Create a gt table for safety outcomes
#'
#' @description
#' Generates a formatted `gt` table summarizing safety outcomes (e.g. adverse
#' events, admissions, deaths, or suicide attempts) by treatment group and total. 
#' Column spanners, widths, and styling (indentation, bold rows, border lines) are 
#' applied automatically based on the structure of the input data.
#'
#' @param ae_dt `data.table` with columns `c("text", "indent_level",
#'   "N_<trt>", "pct_<trt>", "E_<trt>")` where `<trt>` refers to
#'   dexamethasone, placebo, and total groups.
#'
#' @return A `gt` table object with spanners, styled columns, and formatted rows.
#' 
#' @example 
#' 
#' # Get and format event data
#' de <- readRDS("02_data_derived/adverse_events_derived.rds")
#' dt_tab <- mk_ae_dt(de)
#' 
#' # Create table with gt
#' mk_ae_gt(dt_tab)
#' 
mk_ae_gt <- function(ae_dt) {
  # Get treatment levels
  dexa_col    <- sub("N_", "", x = grep("^N_[Dd]exameth", names(ae_dt), value = TRUE))
  placebo_col <- sub("N_", "", x = grep("^N_[Pp]lacebo",  names(ae_dt), value = TRUE))
  
  # Get column names for treatment groups (N, pct, E)
  dexa_N      <- paste0("N_", dexa_col)
  dexa_E      <- paste0("E_", dexa_col)
  dexa_pct    <- paste0("pct_", dexa_col)
  placebo_N   <- paste0("N_", placebo_col)
  placebo_E   <- paste0("E_", placebo_col)
  placebo_pct <- paste0("pct_", placebo_col)
  
  # Add spacer columns to data
  ae_dt <- copy(ae_dt)
  ae_dt[, spacer1 := ""]
  ae_dt[, spacer2 := ""]
  
  # Initiate gt table 
  use_cols <- c("text", dexa_N, dexa_pct, dexa_E, "spacer1",
                placebo_N, placebo_pct, placebo_E, "spacer2",
                "N_Total", "pct_Total","E_Total")
  ae_dt[, .SD, .SDcols = use_cols] |>
    gt() |> 
    # Add spanners 
    tab_spanner(label = dexa_col,    columns = c(dexa_N, dexa_pct, dexa_E)) |>
    tab_spanner(label = placebo_col, columns = c(placebo_N, placebo_pct, placebo_E)) |>
    tab_spanner(label = "Total",     columns = c("N_Total", "pct_Total", "E_Total")) |>
    # Define column names
    cols_label(
      .list = c(
        list(text = "", spacer1 = "", spacer2 = ""),
        setNames(
          rep(c("N", "%", "E"), 3),
          c(dexa_N, dexa_pct, dexa_E,
            placebo_N, placebo_pct, placebo_E,
            "N_Total", "pct_Total", "E_Total")
        )
      )
    ) |>
    # # Define column widths
    # cols_width(
    #   text                ~ pct(50),
    #   spacer1             ~ pct(2.5),
    #   spacer2             ~ pct(2.5),
    #   starts_with("pct_") ~ pct(6),
    #   starts_with("N_")   ~ pct(4.5),
    #   starts_with("E_")   ~ pct(4.5)) |>
    # Use gt options from set_gt_style()
    set_gt_style() |>
    # Left align first column
    tab_style(
      style = cell_text(align = "left"),
      locations = list(
        cells_body(columns = text),
        cells_column_labels(columns = text)
      )
    ) |>
    # Indent rows with indent_level == 1
    tab_style(
      style = cell_text(indent = px(20)),
      locations = cells_body(
        columns = text,
        rows = ae_dt$indent_level == 1
      )
    ) |>
    # Bold text for rows with indent_level == 0
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = ae_dt$indent_level == 0
      )
    ) |>
    # Make line under Safety analysis set
    tab_style(
      style = cell_borders(sides = "bottom",
                           color = "#cccccc",
                           style = "solid",
                           weight = px(1)),
      locations = cells_body(rows = ae_dt$text == "Safety analysis set")
    ) |>
    # Include border line under spanner columns
    tab_options(
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.color = "#cccccc",
      column_labels.border.bottom.width = px(1)
    )
}

#' Add footer notes to a gt table
#'
#' @param gt_table A `gt` table object.
#' @param footer_list A list of character strings, each added as a source note.
#'
#' @return A `gt` table object with footer notes added.
#' 
#' @example
#' 
#' # Get and format derived adverse events data to gt
#' de <- readRDS(file = "02_data_derived/adverse_events_derived.rds")
#' de_tab <- mk_ae_dt(de)
#' gt_tab <- mk_ae_gt(de_tab)
#' 
#' # Define footer
#' footers <- list(scale = "Summary of adverse events", abbrev = "N: Number of individuals.")
#'                
#' # Add footer list to gt table
#' res <- add_footer_list_to_gt(gt_tab, footers)
#' res
#' 
add_footer_lines_to_gt <- function(gt_table, footer_list){
  for(footer in footer_list) {
    gt_table <- gt_table |>
      tab_source_note(source_note = md(as.character(footer)))
  }
  gt_table
}

#' Create summary rows for an adverse event characteristic
#'
#' @description
#' Generates a `data.table` in wide format summarizing counts, percentages,
#' and event numbers for a adverse events by treatment group and total.
#' The output is formatted for direct use with `mk_ae_gt()`.
#'
#' @param event_data `data.table` containing individual-level adverse event 
#' data (one row per adverse event). Must include columns `c("ID", "trt")` and the 
#' column specified by `covariate`.
#' @param covariate `character (1)` name of a factor column in `event_data` to 
#' summarize by. The column must be a factor, as the factor levels determine the 
#' rows in the output table. If missing values are present, a `"Missing data"` 
#' level is added automatically.
#' @param covariate_text `character (1)` display label used as the header row for the 
#' covariate in the output table.
#' 
#' @return A `data.table` with columns `c("high_level_text", "text",
#'   "indent_level", "N_<trt>", "pct_<trt>", "E_<trt>")` where `<trt>`
#'   refers to each treatment group and total.
#'   
#' @example 
#' 
#' # Get event data
#' de <- readRDS("02_data_derived/adverse_events_derived.rds")
#' 
#' # Summarize severity
#' mk_ae_rows(de, "gradering", "Severity")
#' 
mk_ae_rows <- function(event_data, 
                       covariate, 
                       covariate_text) {
  test_required(c("ID", "trt", covariate), names(event_data))
  # Number of individuals in SAS
  N_trt <- mk_sas_summary()
  
  # Number of individuals and number of adverse events
  result_trt <- event_data[, .(N = uniqueN(ID), E = .N), by = c(covariate, "trt")][
    N_trt, pct := 100 * (N / N_total), on = "trt"]
  result_total <- event_data[, .(trt = "Total", N = uniqueN(ID), E = .N), by = covariate][
    N_trt, pct := 100 * (N / N_total), on = "trt"]
  result <- rbind(result_trt, result_total)
  # Indicator of indent for gt table
  result[, indent_level := 1]
  
  # Ensure text is a factor and add "Missing data" as a level if missing data exists in
  # the data
  all_levels <- levels(event_data[[covariate]])
  if (is.null(all_levels)) {
    stop(sprintf("Column '%s' in event_data must be a factor, but is %s", 
                 covariate, class(event_data[[covariate]])))
  }
  if (any(is.na(event_data[[covariate]]))) {all_levels <- c(all_levels, "Missing data")}
  result[, text := fifelse(is.na(get(covariate)), "Missing data", as.character(get(covariate)))]
  result[, text := factor(text, levels = all_levels)]
  
  # Variable needed for gt table
  result[, high_level_text := covariate]
  
  # Pivot to wide format
  result_wide <- dcast(result, high_level_text + text + indent_level ~ trt, 
                       value.var = c("pct", "N", "E"), fill = 0, drop = FALSE)
  
  # Format percent variable
  n_pct_cols <- grep("^pct", names(result_wide), value = TRUE)
  result_wide[, (n_pct_cols) := lapply(.SD, function(x) paste0(do_format(x, 1), "%")), 
              .SDcols = n_pct_cols]
  
  
  
  # Add header for rows
  result_wide <- rbind(result_wide, 
                       data.table(high_level_text = covariate, 
                                  text = covariate_text, 
                                  indent_level = 0), 
                       fill = TRUE)
  
  # Replace NA with "" for nicer gt tables
  num_cols <- grep("^(N_|E_|pct_)", names(result_wide), value = TRUE)
  result_wide[, (num_cols) := lapply(.SD, function(col) ifelse(is.na(col), "", col)), 
              .SDcols = num_cols]
  
  # Order indent level
  setorder(result_wide, indent_level)
  result_wide[]
}

#' Create a summary data.table for adverse events
#'
#' @description
#' Combines multiple safety outcome summaries (seriousness, severity,
#' relationship to trial product, premature discontinuation, duration, and
#' outcome) into a single `data.table` formatted for use with `mk_ae_gt()`.
#' Optionally prepends a Safety Analysis Set (SAS) header row.
#'
#' @param event_data `data.table` containing individual-level adverse event
#'   data.
#' @param sas_header `logical` indicating whether to prepend a SAS header
#'   row generated by `mk_sas_row()`. Defaults to `TRUE`.
#'
#' @return A `data.table` with columns `c("high_level_text", "text",
#'   "indent_level", "N_<trt>", "pct_<trt>", "E_<trt>")` where `<trt>`
#'   refers to each treatment group and total.
#'   
#' @example 
#' 
#' # Get event data
#' de <- readRDS("02_data_derived/adverse_events_derived.rds")
#' 
#' # Make summary data table of adverse events
#' mk_ae_dt(de)
#' 
mk_ae_dt <- function(event_data, 
                     sas_header = TRUE){
  # Any adverse events
  any <- mk_any_ae_row(event_data)
  # Serious
  ser <- mk_ae_rows(event_data, "investigator_alvorlighed", "Serious")
  # Severity
  grad <- mk_ae_rows(event_data, "gradering", "Severity")
  # Relation to trial product
  rel <- mk_ae_rows(event_data, "investigator_relation", "Relationship to trial product")
  # Premature discontinuation
  prem <- mk_ae_rows(event_data, "handling", "Leading to premature treatment discontinuation")
  # Duration
  dur <- mk_ae_rows(event_data, "f_length", "Duration of event")

  dt <- rbind(any, ser, grad, rel, prem, dur)

  # Add SAS header row
  if (sas_header) {
    dt <- rbind(mk_sas_summary(wide_format = TRUE), dt, fill = TRUE)
  }
  
  dt[]
}

#' Create summary rows for MedDRA-coded safety outcomes
#'
#' @description
#' Generates a `data.table` in wide format summarizing counts, percentages,
#' and event numbers for MedDRA-coded adverse events by treatment group and
#' total. The output is formatted for direct use with `mk_ae_gt()`.
#'
#' @param event_data `data.table` containing individual-level event data (one row 
#' per adverse event). Must include columns `c("ID", "trt")` and the columns 
#' specified by `c("high_level", "sub_level")`.
#' @param high_level `character(1)` name of the column in `event_data` containing 
#' the high-level MedDRA term (e.g. `"meddra_organ"`).
#' @param sub_level `character(1)` name of the column in `event_data` containing 
#' the sub-level MedDRA term (e.g. `"meddra_pt"`). If `NULL`, only high-level terms 
#' are summarized. Defaults to `NULL`.
#'
#' @return A `data.table` with columns `c("high_level_text", "text",
#'   "indent_level", "N_<trt>", "pct_<trt>", "E_<trt>")` where `<trt>`
#'   refers to each treatment group and total.
#' 
#' @example 
#' 
#' # Get event data
#' de <- readRDS("02_data_derived/adverse_events_derived.rds")
#' 
#' # Summarize adverse events by MedDRA system organ class
#' mk_meddra_rows(de, high_level = "meddra_organ")
#' 
mk_meddra_rows <- function(event_data, 
                           high_level, 
                           sub_level = NULL) {
  test_required(c("ID", "trt", high_level, sub_level), names(event_data))
  # Number of individuals in SAS
  N_trt <- mk_sas_summary()
  
  # Extract N, % and E for treatment groups and total by high level
  result_trt <- event_data[, .(N = uniqueN(ID), E = .N), by = c(high_level, "trt")][
    N_trt, pct := 100 * (N / N_total), on = "trt"]
  result_total <- event_data[, .(trt = "Total", N = uniqueN(ID), E = .N), by = c(high_level)][
    N_trt, pct := 100 * (N / N_total), on = "trt"]
  result <- rbind(result_trt, result_total)
  
  # Add indent level (for gt table)
  result[, indent_level := 0]
  
  # Add text column (for gt table)
  result[, text := result[[high_level]]]
  
  # Extract N, % and E for treatment groups and total by sub level
  if (!is.null(sub_level)){
    result_trt <- event_data[, .(N = uniqueN(ID), E = .N), by = c(sub_level, high_level, "trt")][
      N_trt, pct := 100 * (N / N_total), on = "trt"]
    result_total <- event_data[, .(trt = "Total", N = uniqueN(ID), E = .N), by = c(sub_level, high_level)][
      N_trt, pct := 100 * (N / N_total), on = "trt"]
    result <- rbind(result_trt, result_total)
    
    result[, indent_level := 1]
    result[, text := result[[sub_level]]]
  }
  
  # Add high_level_text to gt table
  result[, high_level_text := result[[high_level]]]
  
  # Pivot to wide format
  result_wide <- dcast(result, high_level_text + text + indent_level ~ trt, 
                       value.var = c("pct", "N", "E"), fill = 0)
  
  # Ensure all treatment levels are represented with columns
  expected_trts <- c(unique(N_trt$trt))
  expected_cols <- c(
    paste0("N_",   expected_trts),
    paste0("E_",   expected_trts),
    paste0("pct_", expected_trts)
  )
  
  # Include missing treatment columns as 0
  missing_trt_cols <- setdiff(expected_cols, names(result_wide))
  result_wide[, (missing_trt_cols) := 0]
  
  # Format percent
  n_pct_cols <- grep("^pct", names(result_wide), value = TRUE)
  result_wide[, (n_pct_cols) := lapply(.SD, function(x) paste0(do_format(x, 1), "%")),
              .SDcols = n_pct_cols]
  
  # Return data.table
  result_wide[]
}

#' Create a summary data.table for MedDRA-coded adverse events
#'
#' @description
#' Combines organ class and prefered terms MedDRA classification summaries into 
#' a `data.table` formatted for use with `mk_ae_gt()`. Optionally prepends a
#' Safety Analysis Set (SAS) header row and sorts rows either alphabetically
#' or by number of events.
#'
#' @param event_data `data.table` containing individual-level adverse event
#'   data.
#' @param high_level `character(1)` name of the column in `event_data`
#'   containing the organ class MedDRA classification Defaults to `"meddra_organ"`.
#' @param sub_level `character(1)` name of the column in `event_data`
#'   containing the prefered term MedDRA classification  Defaults to `"meddra_pt"`.
#' @param sort_alpha `logical` indicating whether to sort rows alphabetically
#'   by organ class If `FALSE`, rows are sorted by total number of
#'   events in descending order. Defaults to `TRUE`.
#' @param sas_header `logical` indicating whether to prepend a SAS header
#'   row generated by `mk_sas_row()`. Defaults to `TRUE`.
#'
#' @return A `data.table` with columns `c("high_level_text", "text",
#'   "indent_level", "N_<trt>", "pct_<trt>", "E_<trt>")` where `<trt>`
#'   refers to each treatment group and total.
#'   
#' @example 
#' 
#' # Get event data
#' de <- readRDS("02_data_derived/adverse_events_derived.rds")
#' 
#' # Summarize adverse events by MedDRA classfication
#' mk_meddra_dt(de)
#' 
mk_meddra_dt <- function(event_data, 
                         high_level = "meddra_organ", 
                         sub_level  = "meddra_pt", 
                         sort_alpha = TRUE, 
                         sas_header = TRUE){
  # Any adverse event
  any <- mk_any_ae_row(event_data)
  # Make high level rows
  high_level_rows <- mk_meddra_rows(event_data = event_data, high_level)
  # Make sub level rows
  sub_level_rows <- mk_meddra_rows(event_data = event_data, high_level, sub_level)
  
  # Collect data
  result <- rbind(any, high_level_rows, sub_level_rows)
  
  # Add SAS header row
  if (sas_header) {
    result <- rbind(mk_sas_summary(wide_format = TRUE), result, fill = TRUE)}
  
  # Sort alphabetically or by most total events
  if (sort_alpha) {
    result[order(high_level_text, -N_Total)][]
  } else {
    top_order <- result[indent_level == 0][order(-N_Total)]
    top_order[, top_rank := .I]
    
    result[top_order, top_rank := i.top_rank, on = "high_level_text"][
      order(top_rank, indent_level, -N_Total)][]
  }
}

#' Create summary rows for a single safety outcome variable
#'
#' @description
#' Generates a `data.table` summarizing counts, percentages, and event numbers 
#' for a safety outcome variable by treatment group and total. Handles both numeric 
#' outcomes (e.g. number of hospital contacts) and categorical outcomes (e.g. cause 
#' of death). The output is formatted for direct use
#' with `mk_ae_gt()`.
#'
#' @param outcome_data `data.table` containing individual-level outcome data (one 
#'   row per individual).Must include columns `c("ID", "trt")` and the column specified in
#'   `covariate`.
#' @param covariate `character(1)` name of the outcome column in
#'   `outcome_data` to summarize. Either a numeric column where non-zero
#'   values represent number of events, or a factor column where each level generates 
#'   a separate row. The column must be a factor if categorical, as factor levels 
#'   determine the rows in the output table.
#' @param covariate_text `character(1)` display label used as the row label
#'   in the output table.
#' @param indent_level `integer(1)` indentation level for the rows in the
#'   output table. `0` renders non-indented and bold, `1` renders indented.
#'   Defaults to `1`.
#'
#' @return A `data.table` with columns `c("high_level_text", "text",
#'   "indent_level", "N_<trt>", "pct_<trt>", "E_<trt>")` where `<trt>`
#'   refers to each treatment group and total.
#'   
#' @example 
#' 
#' # Get data
#' dt <- readRDS("02_data_derived/admissions_and_suicides_derived.rds")
#' 
#' # Summarize somatic admissions
#' mk_outcome_rows(dt, "soma_count", "Somatic admissions")
#' 
mk_outcome_rows <- function(outcome_data, 
                            covariate, 
                            covariate_text, 
                            indent_level = 1){
  test_required(c("ID", "trt", covariate), names(outcome_data))
  # Number of individuals in SAS
  N_trt <- mk_sas_summary()
  
  if (is.numeric(outcome_data[[covariate]])) {
    # Numeric variable: count non-zero for N, sum for E
    non_zero_subset <- outcome_data[get(covariate) != 0]
    
    if (nrow(non_zero_subset) == 0) { # Make rows despite of no records in data
      result   <- copy(N_trt)[, `:=`(N = 0L, E = 0L, pct = 0L)]
    } else {
      result_trt <- non_zero_subset[, .(N = uniqueN(ID), E = sum(get(covariate), na.rm = TRUE)), by = "trt"][
        N_trt, pct := 100 * (N / N_total), on = "trt"]
      
      result_total <- non_zero_subset[, .(trt = "Total", N = uniqueN(ID), E = sum(get(covariate), na.rm = TRUE))][
        N_trt, pct := 100 * (N / N_total), on = "trt"]
      result <- rbind(result_trt, result_total, fill = TRUE)
    }
    
    result[, (covariate) := covariate_text]
    
  } else {
    # Categorical variable: one row per level
    result_trt   <- outcome_data[!is.na(get(covariate)), 
                                 .(N = uniqueN(ID), E = .N), by = c(covariate, "trt")][
                                   N_trt, pct := 100 * (N / N_total), on = "trt"]
    result_total <- outcome_data[!is.na(get(covariate)), 
                                 .(trt = "Total", N = uniqueN(ID), E = .N), by = covariate][
                                   N_trt, pct := 100 * (N / N_total), on = "trt"]
    result <- rbind(result_trt, result_total)
    
    if (nrow(result) == 0) { # Make rows despite of no records in data
      result <- copy(N_trt)[, `:=`(N = 0L, E = 0L, pct = 0L)]
      result[, (covariate) := factor(NA, levels = levels(outcome_data[[covariate]]))]
    }
    
    result[, (covariate) := factor(get(covariate), levels = levels(outcome_data[[covariate]]))]
  }
  
  # Columns needed for gt table
  result[, indent_level := indent_level]
  result[, text := result[[covariate]]]
  result[, high_level_text := covariate]
  
  # Ensure treatment is a factor
  result[, trt := factor(trt, levels = N_trt$trt)]
  
  # Pivot to wide format
  result_wide <- dcast(result, high_level_text + text + indent_level ~ trt,
                       value.var = c("pct", "N", "E"), fill = 0, drop = FALSE)
  
  # Format pct
  n_pct_cols <- grep("^pct", names(result_wide), value = TRUE)
  result_wide[, (n_pct_cols) := lapply(.SD, function(x) paste0(do_format(x, 1), "%")),
              .SDcols = n_pct_cols]
  
  result_wide[]
}

#' Create a summary table for admissions, suicide attempts and death
#'
#' @description
#' Combines summaries of hospital contacts, suicide attempts, and deaths into
#' a `data.table` formatted for use with `mk_ae_gt()`. Optionally restricts 
#' outcomes to within 28 days of randomization and prepends a Safety Analysis Set 
#' (SAS) header row.
#'
#' @param outcome_data `data.table` containing individual-level outcome data (one 
#' row per individual). Must include columns `c("hc_count", "pam_count",
#'   "psyk_count", "soma_count", "suic_count", "death", "type_doed")`
#'   with or without the suffix `"28"` depending on `day28`.
#' @param sas_header `logical` indicating whether to prepend a SAS header
#'   row generated by `mk_sas_row()`. Defaults to `TRUE`.
#' @param day28 `logical` indicating whether to restrict outcomes to within
#'   28 days of randomization by using columns suffixed with `"28"`
#'   (e.g. `"hc_count28"`, `"death28"`). Defaults to `TRUE`.
#'
#' @return A `data.table` with columns `c("high_level_text", "text",
#'   "indent_level", "N_<trt>", "pct_<trt>", "E_<trt>")` where `<trt>`
#'   refers to each treatment group and total.
#'   
#' @exmaple
#' 
#' # Get data
#' dt <- readRDS("02_data_derived/admissions_and_suicides_derived.rds")
#' 
#' # Summarize admissions, suicide attempts and deaths at day 28
#' mk_outcome_dt(dt)
#' 
mk_outcome_dt <- function(outcome_data, 
                          sas_header = TRUE, 
                          day28      = TRUE) {
  # Select correct columns based on day28 argument
  suffix <- if(day28) "28" else ""
  
  # Hospital contacts
  hc    <- mk_outcome_rows(outcome_data, 
                           paste0("hc_count", suffix), "Hospital contacts",
                           indent_level = 0)
  pam   <- mk_outcome_rows(outcome_data, 
                           paste0("pam_count", suffix), "Psychiatric emergency department visits", 
                           indent_level = 1)
  psyk  <- mk_outcome_rows(outcome_data, 
                           paste0("psyk_count", suffix), "Psychiatric admissions", 
                           indent_level = 1)
  soma  <- mk_outcome_rows(outcome_data, 
                           paste0("soma_count", suffix), "Somatic admissions",
                           indent_level = 1)
  
  # Suicide attempts
  suic  <- mk_outcome_rows(outcome_data, 
                           paste0("suic_count", suffix), "Suicide attempts",
                           indent_level = 0)
  
  # Deaths
  death   <- mk_outcome_rows(outcome_data, 
                             paste0("death", suffix), "Deaths",
                             indent_level = 0)
  death_type  <- mk_outcome_rows(outcome_data, 
                                 paste0("type_doed", suffix), "Cause of death",
                                 indent_level = 1)
  
  # Collect data
  dt <- rbind(hc, pam, psyk, soma, suic, death, death_type, fill = TRUE)
  
  # Add SAS header row
  if(sas_header)
    dt <- rbind(mk_sas_summary(wide_format = TRUE), dt, fill = TRUE)
  
  dt[]
}


