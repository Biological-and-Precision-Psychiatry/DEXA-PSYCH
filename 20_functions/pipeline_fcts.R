

#' Make and save all standard analysis tables
#'
#' @param outc_var outcome variable to make and save tables for (string)
#' @param tlf_reg metadata `data.table` object (cf. `get_metadata()`)
#' @param dt analysis dataset (cf. `get_outcome_data()`)
#' @param set Analysis set
#' @param estimand 
#' @param db_dir directory in which to save an `rds`-file with analysis results. 
#' The directory is made if it does not already exist.
#' @param verbose print information on which tables are saved? (logical scalar)
#'
#' @returns invisibly returns the list of `data.table`s with analysis results 
#' being saved. Each analysis table is named by it's endpoint id.
#'
#' @examples
#' 
#' dttabs <- save_all_dt_tables("mdi_total", set = "FAS")
#' names(dttabs)
#' 
save_all_dt_tables <- function(outc_var = "madrs_total10",
                               tlf_reg  = NULL,
                               dt       = NULL,
                               set      = c("FAS", "SAS"),
                               estimand = c("policy", "efficacy", "safety"),
                               db_dir   = "50_output_db/outputs",
                               verbose  = FALSE) {
  # Make tlf_reg if not provided:
  if(is.null(tlf_reg)) tlf_reg  <- get_metadata()
  
  # Check and match arguments:
  stopifnot(is.character(outc_var) && length(outc_var) == 1L,
            is.data.table(tlf_reg),
            outc_var %in% tlf_reg[, outcome_var],
            is.logical(verbose) && length(verbose) == 1L,
            is.null(db_dir) || (is.character(db_dir) && length(db_dir) == 1L))
  Estimand <- match.arg(estimand)
  set      <- match.arg(set)
  
  # Get outcome data 
  if(is.null(dt)) 
    dt <- get_outcome_data(outc_var, tlf_reg, set = set, estimand = Estimand)
  # Post-baseline dataset for model fitting
  dtpbl <- dt[n_target >= 4] |> droplevels()
  
  # Make all dt tables
  eids <- tlf_reg[estimand == Estimand & outcome_var == outc_var & 
                    tlf_type == "table", endpoint_id]
  all_res <- lapply(setNames(eids, eids), function(eid) { # eid <- eids[1]
    mk_std_table(tlf_reg[endpoint_id == eid], 
                 dt, 
                 dtpbl, 
                 verbose = verbose)
  })
  
  # Save table to file if not db_dir is NULL:
  if(!is.null(db_dir)) {
    if(!dir.exists(db_dir)) dir.create(db_dir)
    db_filename <- file.path(gsub("/$", "", db_dir), 
                             paste0(outc_var, "_", Estimand, ".rds"))
    saveRDS(all_res, file = db_filename)
  }
  
  invisible(all_res)
}


#' Make standard analysis table 
#' 
#' Dispatch a metadata row to the right analysis table function.
#'
#' @param regi a metadata row (cf. `get_metadata()`)
#' @param dt a dataset (cf. `get_outcome_data()`)
#' @param dtpbl a post-baseline dataset
#' @param estimand 
#' @param verbose 
#'
#' @returns a `data.table` with statistical analysis.
#'
#' @examples
#' 
#' # Get metadata and data:
#' tlf_reg  <- get_metadata()
#' outc_var <- "madrs_total10"
#' regi <- tlf_reg[outcome_var == outc_var & imputation == "sipw"][1L]
#' dt <- get_outcome_data(outc_var, set = "FAS")
#' 
#' # Generate statistical analysis table for this regi:
#' mk_std_table(regi, dt) |> display_dttab()
#' 
#' outc_var <- "hamd_total17"
#' regi <- tlf_reg[outcome_var == outc_var][4L]
#' dt <- get_outcome_data(outc_var, set = "FAS")
#' 
#' # Generate statistical analysis table for this regi:
#' mk_std_table(regi, dt) |> display_dttab()
#' 
#' # Example for a censored variable:
#' dt <- get_outcome_data("crp_crp", set = "SAS", estimand = "safety")
#' regi_all <- tlf_reg[outcome_var == "crp_crp" & tlf_type == "table"]
#' regi <- regi_all[2]
#' mk_std_table(regi, dt) |> display_dttab()
#' 
#' regi <- regi_all[1]
#' mk_std_table(regi, dt) |> display_dttab()
#' 
mk_std_table <- function(regi, 
                         dt, 
                         dtpbl    = NULL, 
                         verbose  = FALSE) {
  stopifnot(is.data.table(regi) && nrow(regi) == 1L,
            is.data.table(dt)   && nrow(dt) > 0L)
  # NOTE: variables of 'dt' are checked in the respective analysis functions
  #       so we bypass test_required() here.
  
  # Extract parameters from regi:
  analysis   <- regi[, analysis]
  type       <- regi[, outcome_type]
  imputation <- regi[, imputation]
  outc_kind  <- regi[, outcome_kind]
  visit_char <- regi[, paste0("day", visit)]
  outc_var   <- regi[, outcome_var]
  type_arg   <- regi[, oc_kind_label]
  set_arg    <- regi[, subjects]
  trans      <- regi[, transformation]
  log_arg    <- trans == "log"
  subgrp_var <- regi[, subgrp_var]
  subgrp_lab <- regi[, subgrp_label]
  estimand   <- regi[, estimand]
  data_name  <- if(type == "binary") "value" else regi[, data_name]
  all_vis    <- get_valid_visits(regi[, data_variable])
  all_pb_visits <- paste0("day", all_vis[all_vis > 0])
  mmrm_data_name <- paste("mmrm", outc_var, visit_char, data_name, estimand, 
                          set_arg, sep="_")
  
  # Print 
  if(verbose) print(regi[, .(analysis, type, imputation, visit, data_name)])
  
  # Summary tables:
  if(analysis == "summary" && type == "cont") {
    tab <- tlf_mk_summ_obs_data_cont(dt, 
                                     value.name     = data_name,
                                     set            = set_arg, 
                                     transformation = trans)
    if(data_name == "chgbl" && "f_visit" %in% names(tab)) 
      tab <- tab[f_visit != "day0"]
    return(tab)
  }
  if(analysis == "summary" && type == "binary") {
    if(outc_var == "cssrs_suicide_thoughts")
      return(tlf_mk_summ_obs_data_binary(data       = dt, 
                                         set        = set_arg,
                                         type       = "binary",
                                         type_label = "Suicidal thoughts"))
    return(tlf_mk_summ_obs_data_binary(data  = dt, 
                                       set   = set_arg,
                                       type  = type_arg))
  }
  
  # Get post-baseline and subgroup data if required:
  if(is.null(dtpbl))   dtpbl <- dt[n_target >= 4] |> droplevels()
  if(has_char(subgrp_var)) {
    dbl <- readRDS(file = "02_data_derived/baselinedata.rds")
    dbl <- dbl[, .SD, .SDcols = c("ID", subgrp_var)]
    # Ensure there are no missing values for the subgroup variable:
    stopifnot(dbl[, sum(is.na(get(subgrp_var))) == 0]) 
    dsubgrp <- merge(dtpbl, dbl, by = "ID", all.x = TRUE)
  }
  
  # Subgroup analyses:
  if(analysis %in% c("ancova", "t-test") && has_char(subgrp_var)) 
    return(fit_wlm_subgroup(dsubgrp, 
                            visits         = visit_char,
                            response       = data_name,
                            subgroup_var   = subgrp_var,
                            subgroup_label = subgrp_lab,
                            set            = set_arg))
  if(analysis == "mmrm" && has_char(subgrp_var)){
    return(fit_mmrm_subgroup(dsubgrp, 
                             visit          = visit_char,
                             response       = data_name,
                             subgroup_var   = subgrp_var,
                             subgroup_label = subgrp_lab,
                             set            = set_arg))
  }
  
  if(analysis == "logistic") {
    tl <- if(outc_var == "cssrs_suicide_thoughts") "Suicidal thoughts" else type_arg
    resp <- "value"
    if(outc_var == "crp_crp") {
      tl <- "CRP \u2265 2"
      dtpbl <- add_cens_value(dtpbl)
      dtpbl[, value2 := 1.0 * (value >= 2)]
      resp <- "value2"
    }
    return(fit_wlr(data       = dtpbl, 
                   response   = resp,
                   visit      = visit_char,
                   type       = "binary", 
                   type_label = tl, 
                   set        = set_arg))
  }  
  
  # Do not process cases with Cumulative IP-weighting:
  if(grepl("^cum|xxxx", imputation)) return(data.table(NULL))
  
  # SIP-Weighted ANCOVA and t-test analyses:
  if(analysis %in% c("ancova", "t-test") & imputation %in% c("sipw"))
    return(fit_wlm(dtpbl,
                   visit        = visit_char,
                   model        = analysis,
                   response     = data_name, 
                   weight_model = "trt + value0", 
                   stabilized   = TRUE,
                   set          = set_arg,
                   log          = log_arg))
  
  # Non-weighted ANCOVA and t-test analyses:
  if(analysis %in% c("ancova", "t-test")) # & imputation == "cca"
    return(fit_wlm(dtpbl, 
                   visit    = visit_char,
                   model    = analysis, 
                   response = data_name,
                   set      = set_arg,
                   log      = log_arg))
  
  # MMRM analyses (cumulative visits up to target visit):
  if(analysis == "mmrm") { 
    restab <- fit_mmrm(dtpbl, 
                       response = data_name,
                       set      = set_arg,
                       visit    = visit_char)
    mmrm_data <- attr(restab, "data")
    saveRDS(mmrm_data, file = 
              paste0("50_output_db/mmrm_data/", mmrm_data_name, ".rds"))
    attr(restab, "data") <- NULL
    return(restab)
  }
  
  # Binary analyses:
  if(analysis %in% c("responder", "remission", "sustain") && imputation == "sipw") {
    return(fit_wlr(dtpbl, 
                   visit        = visit_char,
                   type         = type_arg, 
                   response     = data_name,
                   weight_model = "trt + value0", 
                   stabilized   = TRUE,
                   set          = set_arg))
  }
  if(analysis %in% c("responder", "remission", "sustain")) {
    data_arg <- copy(dtpbl)
    response_arg <- data_name
    if(imputation %in% c("mmrm", "mmrm-uce")) {
      mmrm_data_filename <- 
        paste0("50_output_db/mmrm_data/", mmrm_data_name, ".rds")
      if(!file.exists(mmrm_data_filename)) 
        stop("Cannot find MMRM data file: ", mmrm_data_filename)
      data_arg <- readRDS(mmrm_data_filename)
      response_arg <- "value_mmrm"
    } 
    if(imputation %in% c("cca-uce", "mmrm-uce")) {
      return(fit_uce(data_arg, 
                     visit       = visit_char,
                     type        = type_arg, 
                     response    = response_arg,
                     set         = set_arg,
                     get_confint = FALSE))
    } else { # imputation %in% c("cca", "mmrm")
      return(fit_wlr(data_arg, 
                     visit    = visit_char,
                     type     = type_arg, 
                     response = response_arg,
                     set      = set_arg))
    }
  }

  # Default return:
  data.table(NULL)
}



#' Get outcome data from analysis data
#'
#' Filter the full analysis dataset to a single outcome variable (variable +
#' part), restrict to visits baseline through day 28, and set `value` and 
#' `chgbl` to missing where observations are not in-window.
#'   
#' @param outc_var 
#' @param tlf_reg 
#' @param data 
#' @param set 
#' @param estimand 
#'
#' @returns a `data.table` with data ready for analysis with exactly one row
#' for each combination of visit and ID.
#'
#' @examples
#' 
#' outc_var <- "madrs_total6"
#' dt <- get_outcome_data(outc_var[1], set = "FAS")
#' dt <- get_outcome_data(outc_var[1], set = "FAS", estimand = "efficacy")
#' dt <- get_outcome_data("crp_crp", set = "SAS", estimand = "safety")
#' head(dt)
#' 
get_outcome_data <- function(outc_var, 
                             tlf_reg  = NULL, 
                             data     = NULL, 
                             set      = c("all", "FAS", "SAS"),
                             estimand = c("policy", "efficacy", "safety")) { 
  # Check arguments:
  stopifnot(is.character(outc_var) && length(outc_var) == 1L)
  set      <- match.arg(set)
  estimand <- match.arg(estimand)
  if(is.null(data)) data <- get_analysis_data()
  if(is.null(tlf_reg)) tlf_reg <- get_metadata()

  # Get data_variable and data_part for the outcome_variable:
  stopifnot(outc_var %in% tlf_reg[, outcome_var])
  regi <- tlf_reg[outcome_var == outc_var][1L]

  # Keep baseline (n_target == 0) through day 28:
  dt <- data[variable   == regi[, data_variable] &
               part     == regi[, data_part]    &
               n_target <= 28]
  dt[, to_na := 0.0]
  
  # Set off-treatment data to missing for efficacy estimand:
  if(estimand == "efficacy") {
    # keeping c_on_treat: "yes" and "NR":
    dt <- dt[c_on_treat == "no", to_na := 1]
    # Set values after the on-treatment period (after visit 7) to missing
    # if a subject prematurely discontinued treatment:
    dt[!is.na(d_prem_trt_disc) & n_target > 7 & date > d_prem_trt_disc,
       to_na := 1]
  }

  # Set observations not "in window" to NA:  
  dt[state != "in_window" & n_target > 0, to_na := 1.0]

  # Set value variables to NA where indicated:
  to_na_vars <- c("value", "chgbl", "c_value", "d_value")
  for(var in to_na_vars) if(var %in% names(dt)) dt[to_na == 1, (var) := NA]
  dt[, to_na := NULL]
  
  # Remove all-NA value-variables:
  value_vars <- c("c_value", "d_value", "c_value0", "d_value0")
  var_rm <- sapply(value_vars, function(x) 
    x %in% names(dt) && dt[, all(is.na(get(x)))])
  dt <- dt[, .SD, .SDcols = !names(var_rm[var_rm])]
  
  # Filter subjects included:
  if(set == "FAS") dt <- dt[c_FAS == "yes"]
  if(set == "SAS") dt <- dt[c_SAS == "yes"]
  
  # Require exactly 1 row per subject-visit combination:
  stopifnot(dt[, uniqueN(f_visit) * uniqueN(ID) == .N])
  
  dt[] # returning set = "all"
}



#' Display a `data.table` with statistical results
#' 
#' @param dttab 
#' @param display_cols 
#' @param regi 
#' @param eid 
#'
#' @returns
#'
#' @examples
#' 
#' # Get statistical results:
#' tlf_reg <- get_metadata()
#' outc_var <- "mdi_total"
#' regi <- tlf_reg[outcome_var == outc_var & tlf_type == "table"][2L]
#' dttab <- get_dt_table(regi)
#' 
#' # Display table of statistical results:
#' display_dttab(dttab)
#' 
#' # Note that the colnames of dttab are:
#' colnames(dttab)
#' 
#' # dttab has a 'display_cols' attribute:
#' attr(dttab, "display_cols")
#' 
#' # If we set attribute to NULL all columns are shown:
#' attr(dttab, "display_cols") <- NULL
#' display_dttab(dttab)
#' 
#' # If we set additional arguments we can recover the display: 
#' display_dttab(dttab, regi = regi)
#' display_dttab(dttab, eid = eid)   # less effective
#' display_dttab(dttab, display_cols = get_display_cols(regi))
#' 
display_dttab <- function(dttab, 
                          display_cols = NULL, 
                          regi         = NULL, 
                          eid          = NULL) {
  stopifnot(is.data.table(dttab))
  display_cols_attr <- attr(dttab, "display_cols")
  
  if(!is.null(display_cols)) {
    display_cols <- get_in(display_cols, names(dttab))
    if(length(display_cols) > 0L) 
      return(dttab[, .SD, .SDcols = display_cols])
  }
  
  if(!is.null(display_cols_attr)) {
    display_cols <- get_in(display_cols_attr, names(dttab))
    if(length(display_cols) > 0L) 
      return(dttab[, .SD, .SDcols = display_cols])
  }

  if(!is.null(regi) || !is.null(eid)) {
    if(!is.null(regi)) {
      stopifnot(is.data.table(regi),
                nrow(regi) == 1L)
    } else { # !is.null(eid) 
      tlf_reg <- get_metadata()
      stopifnot(eid %in% tlf_reg[, endpoint_id])
      regi <- tlf_reg[endpoint_id == eid]
    }
    display_cols <- get_display_cols(regi)
    display_cols <- get_in(display_cols, names(dttab))
    if(length(display_cols) > 0L)
      return(dttab[, .SD, .SDcols = display_cols])
  }

  # Return (fallback):
  dttab[]  
}


#' Get default display_cols if not set as an attribute
#' 
#' Attempts to guestimate the right display columns. Will only work for the 
#' most common tables.
#'
#' @param regi a one-row metadata `data.table` (cf. `get_metadata()`)
#'
#' @returns the vector of display column names for a given analysis
#'
#' @examples
#' 
#' # Get statistical results:
#' tlf_reg <- get_metadata()
#' outc_var <- "mdi_total"
#' regi <- tlf_reg[outcome_var == outc_var & tlf_type == "table"][2L]
#' dttab <- get_dt_table(regi)
#' 
#' # Get display columns:
#' get_display_cols(regi)
#' 
#' # Compare to display_cols attributes:
#' attr(dttab, "display_cols")
#' 
#' # Display table of statistical results:
#' display_dttab(dttab)
#' 
#' attr(dttab, "display_cols") <- NULL
#' display_dttab(dttab, display_cols = get_display_cols(regi))
#' 
get_display_cols <- function(regi) {
  stopifnot(is.data.table(regi) && nrow(regi) == 1L)
    
  analysis <- regi[, analysis]
  type     <- regi[, outcome_type]
  set      <- regi[, subjects]
  base_cols <- c("Visit", "Treatment", set, "N", "Miss")
  
  if(type == "cont" && analysis == "summary")
    return(c(base_cols, "Mean(SD)", "Median(IQR)", "Range"))

  if(type == "binary" && analysis == "summary")
    return(c(base_cols, regi[, oc_kind_label]))
  
  if(type == "cont")
    return(c(base_cols, "Est. Mean (95% CI)", "Effect (95% CI)", "p-value"))

  if(type == "binary")
    return(c(base_cols, regi[, oc_kind_label], 
             "Est. Prob (95% CI)", "OR (95% CI)", "p-value"))
  
  warning("Cannot determine 'display_cols' for endpoint ID: ", regi[, endpoint_id])
  character(0L)
}



