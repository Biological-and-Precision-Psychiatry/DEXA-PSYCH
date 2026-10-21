

#' Make summary table of observed continuous data
#' 
#' Make a summary table of observed continuous data such as MADRS scores or 
#' MADRS change-from-baseline values.
#'
#' @param data `data.frame` or `data.table` with all data
#' @param value.name name of value variable in `data` (scalar character)
#' @param by name of by variable, often indicating visits (scalar character)
#' @param treatment name of treatment variable (scalar character)
#' @param by.treatment compute table _by_ categories of the treatment variable? 
#' (logical scalar)
#' @param censored is `value.name` a censored variable? (logical scalar)
#'
#' @returns a `data.table` with summary statistics in columns including `FAS`, 
#' `N`, `Miss`, `Mean(SD)`, `Median(IQR)`, and `Range`
#' 
#' @examples
#' 
#' tlf_reg  <- get_metadata()
#' outc_var <- "mdi_total"
#' dt <- get_outcome_data(outc_var, tlf_reg, set = "FAS")
#' mk_summ_obs_data_cont(dt, value.name = "chgbl", treatment = "trt", 
#'                       by = "f_visit", by.treatment = TRUE)
#'                       
#' outc_var <- "tsh_tsh"
#' dt <- get_outcome_data(outc_var, tlf_reg, set = "FAS")
#' dt[!is.na(value), all(value > 0)]
#' mk_summ_obs_data_cont(dt, value.name = "value", treatment = "trt", 
#'                       by = "f_visit", by.treatment = TRUE)
#'                       
#' mk_summ_obs_data_cont(dt, value.name = "value", treatment = "trt", 
#'                       by = "f_visit", by.treatment = FALSE)
#' 
#' # A censored variable:
#' outc_var <- "crp_crp"
#' dt <- get_outcome_data(outc_var, tlf_reg, set = "SAS")
#' # dt <- add_cens_value(dt) # optional
#' mk_summ_obs_data_cont(dt, 
#'                       value.name = "value", 
#'                       by = "f_visit", 
#'                       treatment = "trt",
#'                       set = "SAS", 
#'                       by.treatment = TRUE,
#'                       censored = TRUE)
#' 
mk_summ_obs_data_cont <- function(data, 
                                  value.name   = "value", 
                                  by           = "f_visit", 
                                  treatment    = "trt",
                                  set          = c("FAS", "SAS"),
                                  by.treatment = TRUE,
                                  censored     = FALSE) {
  # data <- copy(dt); value.name = "value"; by <- "f_visit"; treatment = "trt"; 
  # by.treatment = TRUE; set <- "FAS"; censored <- TRUE
  # data <- copy(dt); value.name = "chgbl"; by <- "f_visit"; treatment = "trt"; by.treatment = TRUE
  
  # Check mandatory arguments:
  stopifnot(inherits(data, "data.frame"))
  stopifnot(length(value.name) == 1L,
            is.character(value.name),
            value.name %in% names(data))
  stopifnot(length(by) == 1L,
            is.character(by),
            by %in% names(data))
  stopifnot(length(treatment) == 1L,
            is.character(treatment),
            treatment %in% names(data))
  stopifnot(length(by.treatment) == 1L,
            is.logical(by.treatment))
  stopifnot("Data must contain one row per visit x ID" = 
              data[, uniqueN(ID) * uniqueN(f_visit) == .N])
  set <- match.arg(set)
  
  # Assign BY-variable
  BY <- if(by.treatment) c(treatment, by) else by
  
  # Make .data
  .data <- copy(data)
  setDT(.data) # ensure .data is a data.table
  if(censored && !"cens_lloq" %in% names(.data)) .data <- add_cens_value(.data)
  
  # Handle value.name
  stopifnot(.data[, is.numeric(get(value.name))])
  .data[, .value := get(value.name)]
  .data[.value > 0, .log_value := log(.value)]

  # Make summary table:
  tab <- .data[, .(Subj   = uniqueN(ID), 
                   N      = sum(!is.na(.value)),
                   Miss   = sum(is.na(.value)), 
                   mean   = mean(.value, na.rm = TRUE), 
                   sd     = sd(.value, na.rm = TRUE), 
                   cv     = sd(.value, na.rm = TRUE) / abs(mean(.value, na.rm = TRUE)),
                   median = median(.value, na.rm = TRUE), 
                   Q1     = quantile(.value, probs = 0.25, type = 7, na.rm = TRUE),
                   Q3     = quantile(.value, probs = 0.75, type = 7, na.rm = TRUE),
                   min    = min(.value, na.rm = TRUE), 
                   max    = max(.value, na.rm = TRUE)), 
               keyby = BY]
  setnames(tab, "Subj", set)

  # Add log-scale summaries to tab:
  tab_log <- .data[, 
                   .(N_log    = sum(!is.na(.log_value)),
                     Miss_log = sum(is.na(.log_value)), 
                     gmean    = exp(mean(.log_value, na.rm = TRUE)),
                     gsd      = exp(sd(.log_value, na.rm = TRUE)),
                     gcv      = sqrt(exp(var(.log_value, na.rm = TRUE)) - 1)),
                   keyby = BY]
  tab <- merge(tab, tab_log, by = BY)

  # Add `treatment` column if not in BY-variable:
  if(!treatment %in% BY) {
    tab[, (treatment) := "Any treatment"]
    setcolorder(tab, "trt")
  }
  
  # Format some quantities
  tab[, `Mean(SD)`       := paste0(do_format(mean, digits = 1), "(",
                                   do_format(sd, digits = 1), ")"), by=.I]
  tab[, `Median(IQR)`    := paste0(do_format(median, digits = 0), 
                                   format_ci(Q1, Q3, digits = 0, sep = "; ")), by=.I]
  tab[, Range            := format_ci(min, max, digits = 0, sep = "; "), by=.I]
  tab[, `gMean(gSD)`     := paste0(do_format(gmean, digits = 1), "(",
                                   do_format(gsd, digits = 1), ")"), by=.I]
  tab[, gCV              := do_format(gcv, digits = 2), by=.I]
  
  # Add/replace some variables for censored data:
  if(censored && value.name == "value") {
    stopifnot(requireNamespace("survival", quietly = TRUE))
    stopifnot("cens_lloq" %in% names(.data))
    tab_cens1 <- .data[, get_cens_median(Surv(time  = .value, 
                                             event = (cens_lloq == ">LLOQ"), 
                                             type  = "left")), keyby = BY]
    tab_cens2 <- .data[, .(">LLOQ"  = sum(!is.na(cens_lloq) & 
                                            cens_lloq == ">LLOQ")
    ), keyby = BY]
    tab_cens <- tab_cens2[tab_cens1]
    tab <- merge(tab[, -c("median", "min", "max")], tab_cens,
                 by = BY)
    tab[, `Median(IQR)` := NULL]
    tab[, Median := median]
    tab[, Range  := paste0("(", min, "; ", max, ")"), by = .I]
  }
  
  # Return
  tab[]
}


#' Format summary table of observed continuous data for TLF-book
#'
#' @param data 
#' @param value.name 
#' @param set 
#' @param transformation `"log"` or `"cens-log"`
#'
#' @returns
#'
#' @examples
#' 
#' outc_var <- "tsh_tsh"
#' dt <- get_outcome_data(outc_var, tlf_reg, set = "SAS")
#' dt[!is.na(value), all(value > 0)]
#' res <- tlf_mk_summ_obs_data_cont(dt, "value")
#' res[, .SD, .SDcols = attr(res, "display_cols")]
#' res <- tlf_mk_summ_obs_data_cont(dt, "value", transformation = "log")
#' res[, .SD, .SDcols = attr(res, "display_cols")]
#' 
#' outc_var <- "leukocytter_leukocytter"
#' dt <- get_outcome_data(outc_var, tlf_reg, set = "SAS")
#' dt[!is.na(value), all(value > 0)]
#' res <- tlf_mk_summ_obs_data_cont(dt, "value")
#' res[, .SD, .SDcols = attr(res, "display_cols")]
#' res <- tlf_mk_summ_obs_data_cont(dt, "value", log = TRUE)
#' res[, .SD, .SDcols = attr(res, "display_cols")]
#' 
#' # A censored variable:
#' outc_var <- "crp_crp"
#' dt <- get_outcome_data(outc_var, tlf_reg, set = "SAS")
#' # dt <- add_cens_value(dt) # optional
#' tlf_mk_summ_obs_data_cont(data          = dt,
#'                           value.name     = "value",
#'                           set            = "SAS",
#'                           transformation = "cens-log") |>
#'   display_dttab()
#' 
tlf_mk_summ_obs_data_cont <- function(data, 
                                      value.name,
                                      set            = c("FAS", "SAS"),
                                      transformation = NULL) {
  # Match transformation
  log <- if(!is.null(transformation) && grepl("log", transformation))
    TRUE else FALSE
  censored <- if(!is.null(transformation) && grepl("cens", transformation))
    TRUE else FALSE
  if(censored)
    data <- add_cens_value(data)
  
  set <- match.arg(set)
  tab_any_treat <- mk_summ_obs_data_cont(data, 
                                         value.name   = value.name, 
                                         treatment    = "trt", 
                                         by           = "f_visit", 
                                         set          = set,
                                         by.treatment = FALSE,
                                         censored     = censored)
  tab_by_treat <- mk_summ_obs_data_cont(data, 
                                        value.name   = value.name, 
                                        treatment    = "trt", 
                                        by           = "f_visit", 
                                        set          = set,
                                        by.treatment = TRUE,
                                        censored     = censored)
  tab3 <- rbind(tab_any_treat, tab_by_treat)
  
  # Get treatment order:
  tab3[, trt_order := 0]
  tab3[grepl("[Pp]lacebo", trt), trt_order := 1]
  tab3[grepl("[Dd]exa", trt), trt_order := 2]
  setorder(tab3, f_visit, trt_order)
  
  # Make columns:  
  tab3[, Visit := gsub("day", "Day ", f_visit)]
  tab3[, Treatment := trt]
  setcolorder(tab3, c("Visit", "Treatment"))
  
  # Redefine N and Miss columns of log is TRUE:
  if(log) {
    tab3[, N := NULL][, Miss := NULL]
    setnames(tab3, c("N_log", "Miss_log"), c("N", "Miss"))
  }
  
  # Set attributes:
  display_cols <- c("Visit", "Treatment", set, "N", "Miss", "Mean(SD)", 
                    "Median(IQR)", "Range")
  if(log && !censored) 
    display_cols <- c("Visit", "Treatment", set, "N", "Miss", "gCV", 
                      "gMean(gSD)", "Range")
  if(log && censored) 
    display_cols <- c("Visit", "Treatment", set, "N", "Miss", ">LLOQ", 
                      "Median", "Range")
  attr(tab3, "display_cols") <- display_cols  

  tab3[]
}


#' Make summary of observed data for binary outcomes
#'
#' @param data 
#' @param value.name 
#' @param by 
#' @param treatment 
#' @param type 
#' @param type_label 
#' @param set 
#' @param by.treatment 
#'
#' @returns a `data.table` with data summary.
#'
#' @examples
#' 
#' # Get data for MADRS10:
#' outc_var <- "madrs_total10"
#' dt <- get_outcome_data(outc_var, set = "FAS", estimand = "efficacy")
#' mk_summ_obs_data_binary(dt, type = "Responder")
#' mk_summ_obs_data_binary(dt, type = "Remission")
#' mk_summ_obs_data_binary(dt, type = "Susta")
#' 
#' # type = "binary" example:
#' outc_var <- "cssrs_suicide_thoughts" 
#' dt <- get_outcome_data(outc_var, set = "SAS", estimand = "safety")
#' mk_summ_obs_data_binary(dt, type = "binary", type_label = "Suicide thoughts")
#' # Observe that for type = "binary" baseline is defined.
#' 
mk_summ_obs_data_binary <- function(
    data, 
    value.name   = "value", 
    by           = "f_visit", 
    treatment    = "trt",
    type         = c("Responder", "Remission", "Sustained response", "binary"),
    type_label   = type,
    set          = c("FAS", "SAS"),
    by.treatment = TRUE) {
  # Check mandatory arguments:
  stopifnot(is.data.table(data),
            length(value.name) == 1L && is.character(value.name),
            value.name %in% names(data),
            data[, is.numeric(get(value.name))],
            length(by) == 1L && is.character(by),
            by %in% names(data),
            length(treatment) == 1L && is.character(treatment),
            treatment %in% names(data),
            length(by.treatment) == 1L && is.logical(by.treatment))
  type <- match.arg(type)
  set  <- match.arg(set)
  
  # data <- copy(dt); value.name = "value"; by <- "f_visit"; treatment = "trt"
  # by.treatment = TRUE; type <- "Responder"; set <- "FAS"; type_label <- type
  # by.treatment = TRUE; type <- "Sustained response"; set <- "FAS"; type_label <- type
  # by.treatment = TRUE; type <- "binary"; set <- "FAS"; type_label <- "Suicide thoughts"

  required_var <- c(value.name, "trt", "f_visit", "c_FAS", "c_SAS", "ID", "value0")
  test_required(required_var, names(data), name = "data")
  .d <- data[, .SD, .SDcols = required_var]
  if(type != "binary") stopifnot(value.name == "value") # assumed by get_binary_outcome()
  stopifnot("Data must contain one row per visit x ID" = 
              .d[, uniqueN(ID) * uniqueN(f_visit) == .N])

  ## Assign BY-variable
  BY <- if(by.treatment) c(treatment, by) else by

  ## Get Y and cens variables for post-baseline data:
  .d <- get_binary_outcome(.d, type = type, response = value.name,
                              keep.baseline = type == "binary")
  setorder(.d, ID, f_visit)
  
  ## Get [FS]AS, N, MISS, MEDIAN:
  tab_set <- if(set == "FAS") {
    data[c_FAS == "yes", .(FAS = uniqueN(ID)), keyby=BY]
  } else {
    data[c_SAS == "yes", .(SAS = uniqueN(ID)), keyby=BY]
  }
  tab_N <- .d[, .(N = sum(cens == 0),
                     Miss = sum(cens == 1),
                     nY = sum(Y[cens == 0])), keyby=BY]
  setnames(tab_N, "nY", type_label)
  tab <- tab_set[tab_N]
  tab[]
  
  # Add treatment (`trt`) column if not in BY-variable:
  if(!treatment %in% BY) {
    tab[, (treatment) := "Any treatment"]
    setcolorder(tab, "trt")
  }

  # Return
  tab[]
}

#' Format summary of observed data for binary outcomes for TLF-book
#'
#' @param data 
#' @param value.name 
#' @param set 
#' @param type 
#' @param type_label 
#'
#' @returns
#'
#' @examples
#' 
#' # Get data:
#' outc_var <- "madrs_total10"
#' dt <- get_outcome_data(outc_var, set = "FAS", estimand = "efficacy")
#' tlf_mk_summ_obs_data_binary(dt, type = "Responder")
#' tlf_mk_summ_obs_data_binary(dt, type = "Remission")
#' tlf_mk_summ_obs_data_binary(dt, type = "Susta")
#' tlf_mk_summ_obs_data_binary(dt, type = "Susta") |> display_dttab()
#' 
tlf_mk_summ_obs_data_binary <- function(
    data,
    value.name = "value", 
    set        = c("FAS", "SAS"),
    type       = c("Responder", "Remission", "Sustained response", "binary"),
    type_label = type) {

  type <- match.arg(type)
  set  <- match.arg(set) 
  tab_any_treat <- mk_summ_obs_data_binary(
    data, 
    value.name   = value.name, 
    treatment    = "trt", 
    by           = "f_visit", 
    type         = type,
    type_label   = type_label,
    set          = set,
    by.treatment = FALSE)
  tab_by_treat <- mk_summ_obs_data_binary(
    data, 
    value.name   = value.name, 
    treatment    = "trt", 
    by           = "f_visit", 
    type         = type,
    type_label   = type_label,
    set          = set,
    by.treatment = TRUE)
  tab3 <- rbind(tab_any_treat, tab_by_treat)

  # Get treatment order:
  tab3[, trt_order := 0]
  tab3[grepl("[Pp]lacebo", trt), trt_order := 1]
  tab3[grepl("[Dd]exa", trt), trt_order := 2]
  setorder(tab3, f_visit, trt_order)

  # Make columns:  
  tab3[, Visit := gsub("day", "Day ", f_visit)]
  tab3[, Treatment := trt]
  setcolorder(tab3, c("Visit", "Treatment"))
  
  # Set attributes:
  display_cols <- c("Visit", "Treatment", set, "N", "Miss", type_label) 
  attr(tab3, "display_cols") <- display_cols  
  # tab3[, .SD, .SDcols = display_cols]
  
  tab3[]
}


#' Get min, max, and median of a censored variable
#'
#' @param Y 
#' @param digits 
#'
#' @returns
#'
#' @examples
#' 
#' Y <- dt[, Surv(time=value, event=(cens_lloq == ">LLOQ"), type="left")]
#' get_cens_median(Y)
#' 
get_cens_median <- function(Y, digits = 2) {
  stopifnot(inherits(Y, "Surv"),
            is.numeric(digits) && digits >= 0)
  digits <- round(digits)
  is_even <- function(x) (x %% 2) == 0
  
  .d <- as.data.table(as.matrix(sort(Y)))
  Min <- if(.d[1, status == 1]) 
    do_format(.d[1, time], digits) else paste0("<", do_format(.d[1, time], digits))
  Max <- if(.d[.N, status == 1]) 
    do_format(.d[.N, time], digits) else paste0("<", do_format(.d[.N, time], digits))
  half <- .d[, .N/2]
  if(is_even(.d[, .N])) {
    X <- .d[half:(half+1)]
    if(X[, all(status == 0)]) Median <- paste0("<", do_format(X[1, 1], digits)) else {
      Median <- do_format(X[, mean(time)], digits)
    }
  } else {
    X <- .d[ceiling(half)]
    Median <- if(X[, status == 0]) paste0("<", do_format(X[, time], digits)) else 
      do_format(X[, time], digits)
  }
  (res <- data.table(median = Median, min = Min, max = Max))
}

#' Add censored variable to data
#'
#' @param dt 
#'
#' @returns
#'
#' @examples
add_cens_value <- function(dt) {
  # Check arguments:
  stopifnot(is.data.table(dt) & nrow(dt) > 0L)
  required_cols <- c("c_value", "ID", "n_target")
  test_required(required_cols, names(dt))
  
  # Make censoring indicator (cens_lloq):
  dt[, cens_lloq := NA_character_]
  dt[!is.na(c_value), cens_lloq := fifelse(substr(c_value, 1, 1) == "<", c_value, ">LLOQ")]
  dt[, .N, keyby=cens_lloq] |> totpct()
  
  # Get values of LLOQ for censored data:
  dt[cens_lloq != ">LLOQ", value := comma2numeric(gsub("<", "", cens_lloq))]
  dt[, .N, keyby = .(cens_lloq, value)]
  
  # Get values for uncensored data:
  dt[!is.na(c_value) & cens_lloq == ">LLOQ", value := comma2numeric(c_value)]
  dt[, .N, keyby = .(cens_lloq, is.na(value))]
  
  # Look at data:
  head(dt[cens_lloq != ">LLOQ"])
  head(dt[cens_lloq == ">LLOQ"])
  
  # Artificially censor all values <= 0.3 at 0.3:
  dt[!is.na(value) & value <= .3, `:=`(cens_lloq = "<0.30",
                                       value = 0.3)]
  dt[, .N, keyby = .(cens_lloq, is.na(value))]
  
  # Set and impute baseline values:
  dt[, value0 := value[n_target == 0], ID]
  dt[!is.na(value0) & value0 <= 0.305, value0 := 0.3 / sqrt(2)]
  dt[is.na(value0)]$value0 <- dt[, mean(value0, na.rm = TRUE)]
  
  dt[]  
}
