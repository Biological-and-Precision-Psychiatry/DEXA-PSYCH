
#' Fit a weighted linear model
#'
#' @param data 
#' @param visit 
#' @param model 
#' @param response 
#' @param set 
#' @param log 
#' @param weight_model 
#' @param stabilized 
#'
#' @returns a `data.table` with a `display_tables` attribute.
#'
#' @examples
#' 
#' # Get data and fit model for MADRS10:
#' outc_var <- "madrs_total10"
#' dt <- get_outcome_data(outc_var, set = "FAS", estimand = "policy")
#' fit_wlm(dt, set = "FAS", visit = "day28") |> 
#'   display_dttab()
#' 
#' fit_wlm(dt, set = "FAS", visit = "day4", 
#'                   weight_model = "trt + value0", stabilized = TRUE) |>
#'   display_dttab()
#'   
#' # Get data and fit model for Leucocytes:
#' outc_var <- "leukocytter_leukocytter"
#' dt <- get_outcome_data(outc_var, set = "SAS", estimand = "safety")
#' res <- fit_wlm(dt, set = "SAS", visit = "day7",
#'                          weight_model = "trt + value0", stabilized = TRUE)
#' display_dttab(res)
#' 
#' Safety outcome using stabilized IP weighting:
#' outc_var <- "vaegt_vaegt"
#' dt <- get_outcome_data(outc_var, set = "SAS", estimand = "safety")
#' fit_wlm(dt, set = "SAS", visit = "day7", 
#'                   weight_model = "trt + value0", stabilized = TRUE) |>
#'   display_dttab()
#'   
fit_wlm <- function(data, 
                    visit        = c("day4", "day7", "day14", "day28"),
                    model        = c("ancova", "t-test"), 
                    response     = "value",
                    set          = c("FAS", "SAS"),
                    log          = FALSE, 
                    weight_model = NULL, # "trt + value0"
                    stabilized   = TRUE) {
  # Check arguments:
  stopifnot(is.data.table(data) && nrow(data) > 0L,
            is.character(response) && length(response) > 0L,
            is.logical(log) && length(log) == 1L,
            is.null(weight_model) || 
              (is.character(weight_model) && length(weight_model) == 1L),
            is.logical(stabilized) && length(stabilized) == 1L)
  set <- match.arg(set)
  model <- match.arg(model)
  visit <- match.arg(visit)
  
  # data <- copy(dt); model <- "ancova"; response <- "value"; log <- TRUE; set <- "SAS"
  # data <- copy(dt); model <- "ancova"; response <- "value"; log <- FALSE; set <- "FAS"
  # visit = "day7"; weight_model = "trt + value0"; stabilized = TRUE
  # visit = "day28"; weight_model = NULL; stabilized = FALSE
  
  required_var <- c(response, "trt", "value0", "f_visit", "c_FAS", "c_SAS", "ID")
  test_required(required_var, names(data))
  .d <- data[, .SD, .SDcols = required_var]
  stopifnot("Data must contain one row per visit x ID" = 
              .d[, uniqueN(ID) * uniqueN(f_visit) == .N])
  stopifnot(visit %in% .d[, unique(f_visit)])
  
  ## Format data response and censoring:
  .d[, Y := get(response)]
  if(log) {
    .d[!is.na(Y),           Y := fifelse(Y      <= 0, NA, log(Y))]
    .d[!is.na(value0), value0 := fifelse(value0 <= 0, NA, log(value0))]
  }
  .d[, cens := 1.0 * is.na(Y)]
  vdata <- .d[f_visit == visit] |> droplevels()

  ## Identify treatment levels:  
  trt_levs <- get_trt_levs(vdata)
  ref_trt_lev <- trt_levs[1] |> unname()
  dexa_trt_lev <- trt_levs[2] |> unname()

  ## Get IPC-weights:
  vdata$wt <- get_ipc_weights(vdata, weight_model, stabilized)

  ## Model-formula:
  if(model == "t-test")
    form <- reformulate("trt", response = "Y")
  if(model == "ancova")
    form <- reformulate(c("value0", "trt"), response = "Y")
  
  fm <- lm(form, data = vdata[cens == 0], weights = wt)
  
  ## Compute EMMEANS:
  em <- emmeans_quiet(fm, "trt", data = vdata)
  df_em <- as.data.frame(em) |> as.data.table()
  df_em[, "Est. Mean (95% CI)" := 
          format_estci(emmean, lower.CL, upper.CL, dig=1, ci_sep = "; ", 
                       use_exp = log)]
  df_em
  
  ## Get [FS]AS, N, MISS, MEDIAN:
  tab_set <- if(set == "FAS") {
    .d[c_FAS == "yes", .(FAS = uniqueN(ID)), keyby=trt]
  } else {
    .d[c_SAS == "yes", .(SAS = uniqueN(ID)), keyby=trt]
  }
  tab_N <- vdata[, .(N = if(is.null(weight_model)) sum(cens == 0) else .N,
                     Miss = sum(cens == 1),
                     wt = format(sum(wt[cens == 0]), digits=1)), keyby=trt]
  tab_N
  
  ## Compute EFFECT:
  cem <- contrast(em, "trt.vs.ctrl", ref=ref_trt_lev, infer=TRUE) |>
    as.data.frame() |> as.data.table()
  cem[, `:=`("Effect (95% CI)" = format_estci(estimate, lower.CL, upper.CL, dig=2, 
                                              ci_sep = "; ", use_exp = log),
             `p-value` = format_pval2(p.value),
             trt = dexa_trt_lev)]
  cem
  
  ## Collect and return tables:
  tab_emmeans <- cbind(visit = visit, tab_set[tab_N][df_em])
  tab_effect <- cbind(visit = visit, cem)
  nms <- c("SE", "df", "lower.CL", "upper.CL")
  setnames(tab_emmeans, nms, paste0(nms, ".emmean"))

  res <- merge(tab_emmeans, tab_effect, by=c("visit", "trt"), all.x=TRUE)
  res[, Treatment := trt]
  res[, Visit := gsub("day", "Day ", visit)]
  setorder(res, -trt)
  attr(res, "vcov_em") <- vcov(em, sep = " ")
  display_cols <- c("Visit", "Treatment", set,  "N", "Miss", 
                    "Est. Mean (95% CI)", "Effect (95% CI)", "p-value")
  attr(res, "display_cols") <- display_cols
  res
}


#' Fit a subgroup ANCOVA model
#'
#' @param data 
#' @param visit 
#' @param response 
#' @param subgroup_var 
#' @param subgroup_label 
#' @param set 
#'
#' @returns
#'
#' @examples
#' 
#' # Get data and fit model for MADRS10:
#' outc_var <- "madrs_total10"
#' dt <- get_outcome_data(outc_var, set = "FAS", estimand = "efficacy")
#' dbl <- readRDS(file = "02_data_derived/baselinedata.rds")
#' dt <- merge(dt, dbl[, .(ID, sex)], by = "ID", all.x = TRUE)
#' 
#' # Fit subgroup model:
#' fit_wlm_subgroup(dt, 
#'                  visit          = "day28", 
#'                  subgroup_var   = "sex", 
#'                  subgroup_label = "Sex",
#'                  set            = "FAS") |> 
#'   display_dttab()
#'   
fit_wlm_subgroup <- function(data, 
                             visit          = "day7", 
                             response       = "value", 
                             subgroup_var   = "sex",
                             subgroup_label = "Sex",
                             set            = c("FAS", "SAS")) {
  # Check arguments:
  stopifnot(is.data.table(data) && nrow(data) > 0,
            is.character(visit) && length(visit) >= 1L,
            is.character(response) && length(response) == 1L,
            is.character(subgroup_var) &&  length(subgroup_var) == 1L,
            is.character(subgroup_label) && length(subgroup_label) == 1L)
  required_var <- c(response, subgroup_var, "trt", "value0", "f_visit", 
                    "c_FAS", "c_SAS", "ID")
  test_required(required_var, names(data))
  stopifnot(visit %in% data[, unique(f_visit)])
  alldata <- data[, .SD, .SDcols = required_var]
  stopifnot("Data must contain one row per visit x ID" = 
              alldata[, uniqueN(ID) * uniqueN(f_visit) == .N])
  set <- match.arg(set)
  
  alldata[, Y := get(response)]
  alldata[, Group := factor(get(subgroup_var))]
  alldata[, cens := 1.0 * is.na(Y)]
  vdata <- alldata[f_visit == visit] |> droplevels()
  
  ## Identify treatment levels:  
  trt_levs <- get_trt_levs(vdata)
  ref_trt_lev <- trt_levs[1] |> unname()
  dexa_trt_lev <- trt_levs[2] |> unname()
  
  ## Get [FS]AS, N, MISS, MEDIAN:
  tab_set <- if(set == "FAS") {
    alldata[c_FAS == "yes", .(FAS = uniqueN(ID)), keyby = .(Group, trt)]
  } else {
    alldata[c_SAS == "yes", .(SAS = uniqueN(ID)), keyby = .(Group, trt)]
  }
  tab_N <- vdata[, .(N    = sum(cens == 0),
                     Miss = sum(cens == 1)), 
                 keyby = .(Group, trt)]
  tab_set[tab_N]
  
  ## Fit model:
  fm <- lm(Y ~ value0 + trt * Group, data = vdata)
  
  # Compute EMMEANS:
  cov_reduce <- reformulate("Group", response = "value0")
  em <- emmeans(fm, "trt", by = "Group", data = vdata, cov.reduce = cov_reduce)
  df_em <- as.data.frame(em) |> as.data.table()
  df_em[, "Est. Mean (95% CI)" := 
          format_estci(emmean, lower.CL, upper.CL, dig=1, ci_sep = "; ")]
  setkey(df_em, Group, trt)
  df_em
  
  ## Compute EFFECT:
  cem <- contrast(em, "trt.vs.ctrl", ref=ref_trt_lev, infer=TRUE) 
  het_test <- joint_tests(cem) |> as.data.frame() |> as.data.table()
  df_cem <- as.data.frame(cem) |> as.data.table()
  df_cem[, `:=`("Effect (95% CI)" = 
                  format_estci(estimate, lower.CL, upper.CL, dig=2, ci_sep = "; "),
                `p-value` = format_pval2(p.value),
                trt = dexa_trt_lev)]
  
  # Add interaction p-value (p-het):
  levs <- alldata[, levels(Group)]
  df_cem <- merge(df_cem, 
                  het_test[, .(Group=last(levs), `p-het` = format_pval2(p.value),
                               het.pvalue = p.value)], 
                  by=c("Group"), all.x = TRUE)
  df_cem
  
  # Collect and return tables:
  tab_emmeans <- cbind(subgroup_var = subgroup_var, visit = visit, 
                       tab_set[tab_N][df_em])
  tab_effect <- cbind(visit = visit, df_cem)
  nms <- c("SE", "df", "lower.CL", "upper.CL")
  setnames(tab_emmeans, nms, paste0(nms, ".emmean"))
  
  res <- merge(tab_emmeans, tab_effect, by=c("visit", "trt", "Group"), all.x=TRUE)
  res[, Treatment := trt]
  res[, Visit := gsub("day", "Day ", visit)]
  res[, Group := gsub("<=", "\u2264", Group)]
  res[, Group := gsub(">=", "\u2265", Group)]
  setorder(res, Group, -trt)
  setnames(res, "Group", subgroup_label)
  display_cols <- c("Visit", "Treatment", subgroup_label, set, "N", 
                    "Est. Mean (95% CI)", "Effect (95% CI)", "p-value", "p-het")
  attr(res, "display_cols") <- display_cols
  res
}


#' Fit weighted logistic regression model
#'
#' @param data 
#' @param visit 
#' @param type 
#' @param response 
#' @param set 
#' @param weight_model 
#' @param stabilized 
#'
#' @returns
#'
#' @examples
#' 
#' # Get data and fit logistic regression responder-model for MADRS10 to day 14:
#' outc_var <- "madrs_total10"
#' dt <- get_outcome_data(outc_var, set = "FAS", estimand = "efficacy")
#' fit_wlr(dt) |> display_dttab()
#' 
#' # Remission analysis:
#' fit_wlr(dt, visit = "day4", type = "Remission") |> display_dttab()
#' 
#' # Sustained response analysis:
#' fit_wlr(dt, visit = "day28", type = "Sustain") |> display_dttab()
#'   
#' # CRP analysis:
#' outc_var <- "crp_crp"
#' dt <- get_outcome_data(outc_var, set = "SAS", estimand = "safety")
#' dtpbl <- dt[n_target > 0] |> droplevels()
#' tl <- "CRP \u2265 2"
#' dtpbl <- add_cens_value(dtpbl)
#' dtpbl[, value2 := 1.0 * (value >= 2)]
#' resp <- "value2"
#' fit_wlr(dtpbl, visit = "day7", type = "binary", type_label = tl, 
#'         response = "value2", set = "SAS") |>
#'   display_dttab()
#' 
#' # Example with MMRM-imputed data:
#' mmrm_data_filename <- 
#'   "50_output_db/mmrm_data/mmrm_madrs_total10_day28_value_policy_FAS.rds"
#' if(file.exists(mmrm_data_filename)) {
#'   dt <- readRDS(mmrm_data_filename)
#'   fit_wlr(dt, 
#'           visit    = "day28",
#'           type     = "Sustained response", 
#'           response = "value_mmrm",
#'           set      = "FAS") |> display_dttab()
#' } 
#' # Compare to:
#' outc_var <- "madrs_total10"
#' dt <- get_outcome_data(outc_var, set = "FAS", estimand = "policy")
#' fit_wlr(dt, visit = "day28", type = "Sustain") |> display_dttab()
#' 
#' # Examples with IPC-weighting:
#' outc_var <- "madrs_total10"
#' dt <- get_outcome_data(outc_var, set = "FAS", estimand = "policy")
#' fit_wlr(dt, visit = "day4", type = "Responder") |>  display_dttab()
#' fit_wlr(dt, visit = "day4", type = "Responder", weight_model = "trt + value0") |> 
#'   display_dttab()
#' fit_wlr(dt, visit = "day4", type = "Remission", weight_model = "trt + value0") |> 
#'   display_dttab()
#' fit_wlr(dt, visit = "day4", type = "Responder", weight_model = "trt + value0", 
#'         stabilized = FALSE) |> display_dttab()
#' 
fit_wlr <- function(
    data, 
    visit        = c("day4", "day7", "day14", "day28"),
    type         = c("Responder", "Remission", "Sustained response", "binary"),
    type_label   = type,
    response     = "value", 
    set          = c("FAS", "SAS"),
    weight_model = NULL, # "trt + value0"
    stabilized   = TRUE) {
  ## Check arguments:
  stopifnot(is.data.table(data) && nrow(data) > 0L,
            is.character(response) && length(response) > 0L,
            is.null(weight_model) || 
              (is.character(weight_model) && length(weight_model) == 1L),
            is.logical(stabilized) && length(stabilized) == 1L)
  set  <- match.arg(set)
  type <- match.arg(type) 
  visit <- match.arg(visit)
  if(type == "Sustained response" & !is.null(weight_model) & visit != "day4")
    warning("IP weighting may not work with Sustained Response")
  
  # type <- "Responder"; visit = "day4"; response = "value"; type_label <- type; set <- "FAS"
  # type <- "Remission"; visit = "day4"; response = "value"; type_label <- type; set <- "FAS"
  # type <- "Sustained response"; visit = "day28"; response = "value"; type_label <- type; set <- "FAS"
  # type <- "Sustained response"; visit = "day28"; response = "value_mmrm"; type_label <- type; set <- "FAS"
  # data <- copy(dt); weight_model = NULL; stabilized = TRUE
  # data <- copy(dt); weight_model = "trt + value0"; stabilized = TRUE
  
  ## Get data:
  resp <- unique(c(response, "value"))
  required_var <- c(resp, "trt", "value0", "f_visit", "c_FAS", "c_SAS", "c_ID")
  test_required(required_var, names(data))
  .d <- data[, .SD, .SDcols = required_var]
  stopifnot("Data must contain one row per visit x ID" = 
              .d[, uniqueN(c_ID) * uniqueN(f_visit) == .N])
  .d[, ID := as.numeric(c_ID)] # ID may not exist in MMRM-imputed data
  setorder(.d, ID, f_visit)

  ## Get Y and cens variables for post-baseline data:
  vdata <- get_binary_outcome(.d, type = type, response = response, penalty = 0)
  
  ## Select data on visit:
  vdata <- vdata[f_visit == visit] |> droplevels()
  
  ## Identify treatment levels:  
  trt_levs <- get_trt_levs(vdata)
  ref_trt_lev <- trt_levs[1] |> unname()
  dexa_trt_lev <- trt_levs[2] |> unname()
  if(!vdata[, is.factor(trt)]) vdata[, trt := factor(trt)]
  vdata[, trt := relevel(trt, ref = ref_trt_lev)]

  ## Get IPC-weights:
  vdata$wt <- get_ipc_weights(vdata, weight_model, stabilized)
  
  ## Fit outcome model:
  form <- if(type == "binary") "Y ~ trt" else "Y ~ value0 + trt"
  form <- as.formula(form)
  mdata <- vdata[cens == 0]
  # Suppress warning: 'non-integer #successes in a binomial glm!'
  model <- glm(form, data = mdata, weights = wt, family = binomial) |> 
    suppressWarnings()
  # summary(model)
  
  ## Compute LR-test:
  an <- drop1(model, scope = ~trt, test = "LRT") |> suppressWarnings()
  pval_lrt <- an["trt", grepl("Pr", colnames(an))]
  
  ## Compute profile likelihood CI:
  # Guard against complete separation
  complete_sep <- any(vdata[, table(Y, trt, useNA = "no")] == 0)
  ci_lr <- if(complete_sep) rep(NA_real_, 2) else {
    stats::confint(model, level = 0.95, test = "LRT",
                   parm = getgrep(dexa_trt_lev, names(coef(model)))) |>
      suppressWarnings() |> suppressMessages() |> exp() 
  }
  ci_lr <- setNames(ci_lr, nm = c("lwr.lrt", "upr.lrt")) |> 
    rbind() |> as.data.table()

  ## Get [FS]AS, N, MISS:
  # if response is imputed get Miss from 'value' instead:
  val_name <- if(response == "value_mmrm") "value" else response
  stab <- mk_summ_obs_data_binary(.d, value.name = val_name, by = "f_visit", 
                                  treatment = "trt", type = type, 
                                  type_label = type_label, set = set)
  stab <- stab[f_visit == visit, .SD, keyby = trt, .SDcols = c(set, "Miss")]
  tab_N <- vdata[, .(uncens = sum(cens == 0),
                     N = if(is.null(weight_model)) sum(cens == 0) else .N,
                     wt = sum(wt[cens == 0]),
                     nY = sum(Y[cens == 0])), keyby = trt]
  is_n <- sapply(tab_N, is.numeric)
  nms <- names(is_n[is_n])
  tab_N[, (nms) := lapply(.SD, do_format, digits = 0), .SDcols = nms]
  tab_N[, resp := paste0(nY, "/", uncens)]
  setnames(tab_N, "resp", type_label)
  tab_N
  
  ## Compute EMMEANS:
  em <- emmeans_quiet(model, "trt", type = "response", infer = TRUE, data = vdata)
  df_em <- as.data.frame(em) |> as.data.table()
  df_em[, "Est. Prob (95% CI)" := 
          format_estci(prob, asymp.LCL, asymp.UCL, dig=2, ci_sep = "; ")]
  df_em

  ## Compute EFFECT:
  cem <- contrast(em, "trt.vs.ctrl", ref=ref_trt_lev, infer=TRUE) |>
    as.data.frame() |> as.data.table()
  cem <- cbind(cem, ci_lr, pval_lrt) # Add LR p-value & CI
  cem[, `:=`("OR (95% CI)" = format_estci(odds.ratio, lwr.lrt, upr.lrt, dig=2, 
                                          ci_sep = "; "),
             `p-value`     = format_pval2(pval_lrt),
             trt           = dexa_trt_lev)]

  ## Guard against Undefined Odds-Ratios:
  # Largest possible OR is (149 * 149) / (1 * 1) = 22201, so any estimate 
  # higher than that is Not Defined.
  cem[odds.ratio > 25000 | odds.ratio < 1/25000 | complete_sep, 
      `OR (95% CI)` := gsub("^\\S+", "ND", `OR (95% CI)`)]
  cem
  
  ## Collect tables:
  tab_emmeans <- cbind(visit = visit, stab[tab_N][df_em])
  tab_effect <- cbind(visit = visit, cem)
  nms <- c("SE", "df", "asymp.LCL", "asymp.UCL", "null", "z.ratio", "p.value")
  setnames(tab_emmeans, nms, paste0(nms, ".emmean"))
  res <- merge(tab_emmeans, tab_effect, by=c("visit", "trt"), all.x=TRUE)
  res[, Visit := gsub("day", "Day ", visit)]
  res[, Treatment := trt]
  setorder(res, -trt)
  
  # Set attributes:
  display_cols <- c("Visit", "Treatment", set, "N", "Miss", type_label,
                    "Est. Prob (95% CI)", "OR (95% CI)", "p-value")
  attr(res, "display_cols") <- display_cols  
  
  ## Return
  res
}


#' Perform an Unconditional Exact Test
#'
#' @param data 
#' @param visit 
#' @param type 
#' @param type_label 
#' @param response 
#' @param set 
#'
#' @returns
#'
#' @examples
#' 
#' # Get data and fit logistic regression responder-model for MADRS10 to day 14:
#' outc_var <- "madrs_total10"
#' dt <- get_outcome_data(outc_var, set = "FAS", estimand = "efficacy")
#' fit_uce(dt, get_confint = FALSE) |> display_dttab()
#' 
#' # Example with MMRM-imputed data:
#' mmrm_data_filename <- 
#'   "50_output_db/mmrm_data/mmrm_madrs_total10_day28_value_policy_FAS.rds"
#' if(file.exists(mmrm_data_filename)) {
#'   dt <- readRDS(mmrm_data_filename)
#'   fit_uce(dt, 
#'           visit    = "day28",
#'           type     = "Sustained response", 
#'           response = "value_mmrm",
#'           set      = "FAS") |> display_dttab()
#' } 
#' # Compare to:
#' outc_var <- "madrs_total10"
#' dt <- get_outcome_data(outc_var, set = "FAS", estimand = "policy")
#' fit_uce(dt, visit = "day28", type = "Sustain", get_confint = FALSE) |> 
#'   display_dttab()
#' 
#' fit_uce(dt, visit = "day28", type = "Sustain", get_confint = TRUE) |> 
#'   display_dttab()
#'   
fit_uce <- function( # unconditional exact test
    data, 
    visit        = c("day4", "day7", "day14", "day28"),
    type         = c("Responder", "Remission", "Sustained response", "binary"),
    type_label   = type,
    response     = "value", 
    set          = c("FAS", "SAS"),
    get_confint  = TRUE) {
  stopifnot(require("exact2x2", quietly = TRUE))
  ## Check arguments:
  stopifnot(is.data.table(data) && nrow(data) > 0L,
            is.character(response) && length(response) > 0L,
            is.logical(get_confint) && length(get_confint) == 1L)
  set  <- match.arg(set)
  type <- match.arg(type) 
  visit <- match.arg(visit)

  # type <- "Responder"; visit = "day28"; response = "value"; type_label <- type; set <- "FAS"
  # type <- "Remission"; visit = "day4"; response = "value"; type_label <- type; set <- "FAS"
  # type <- "Sustained response"; visit = "day28"; response = "value"; type_label <- type; set <- "FAS"
  # type <- "Sustained response"; visit = "day28"; response = "value_mmrm"; type_label <- type; set <- "FAS"
  # data <- copy(dt); get_confint <- FALSE

  ## Get data:
  resp <- unique(c(response, "value"))
  required_var <- c(resp, "trt", "value0", "f_visit", "c_FAS", "c_SAS", "c_ID")
  test_required(required_var, names(data))
  .d <- data[, .SD, .SDcols = required_var]
  stopifnot("Data must contain one row per visit x ID" = 
              .d[, uniqueN(c_ID) * uniqueN(f_visit) == .N])
  .d[, ID := as.numeric(c_ID)] # ID may not exist in MMRM-imputed data
  setorder(.d, ID, f_visit)
  
  ## Get Y and cens variables for post-baseline data:
  vdata <- get_binary_outcome(.d, type = type, response = response, penalty = 0)
  
  ## Select data on visit:
  vdata <- vdata[f_visit == visit] |> droplevels()
  
  ## Identify treatment levels:  
  trt_levs <- get_trt_levs(vdata)
  ref_trt_lev <- trt_levs[1] |> unname()
  dexa_trt_lev <- trt_levs[2] |> unname()
  if(!vdata[, is.factor(trt)]) vdata[, trt := factor(trt)]
  vdata[, trt := relevel(trt, ref = ref_trt_lev)]
  
  ## Get [FS]AS, MISS:
  # if response is imputed get Miss from 'value' instead:
  val_name <- if(response == "value_mmrm") "value" else response
  stab <- mk_summ_obs_data_binary(.d, value.name = val_name, by = "f_visit", 
                                  treatment = "trt", type = type, 
                                  type_label = type_label, set = set)
  stab <- stab[f_visit == visit, .SD, keyby = trt, .SDcols = c(set, "Miss")]

  ## Get N, nY, Prop:
  tab_N <- vdata[, .(N = sum(cens == 0),
                     nY = sum(Y[cens == 0])), keyby=trt]
  tab_N[, (type_label) := paste0(nY, "/", N)]
  tab_N[, Prop := do_format(nY / N, digits = 2)]
  tab_N
  
  ## Compute unconditional exact test:
  cem <- get_uce_test(x1 = tab_N[1, nY], n1 = tab_N[1, N], 
                      x2 = tab_N[2, nY], n2 = tab_N[2, N])
  setnames(cem, "estimate", "odds.ratio")
  cem[, trt := factor(dexa_trt_lev)]
  cem[, "p-value" := format_pval2(p.value)]
  cem[, "OR (95% CI)" := format_estci(odds.ratio, lwr_ci, upr_ci, 
                                      digits = 2, ci_sep = "; ")]

  ## Guard against Undefined Odds-Ratios:
  # Largest possible OR is (149 * 149) / (1 * 1) = 22201, so any estimate 
  # higher than that is Not Defined.
  complete_sep <- any(vdata[, table(Y, trt, useNA = "no")] == 0)
  cem[odds.ratio > 25000 | odds.ratio < 1/25000 | complete_sep, 
      `OR (95% CI)` := gsub("^\\S+", "ND", `OR (95% CI)`)]
  cem
  
  ## Collect tables:
  res <- merge(cbind(visit = visit, stab[tab_N]), cem, by = "trt", all.x = TRUE)
  res[, Visit := gsub("day", "Day ", visit)]
  res[, Treatment := trt]

  # Set attributes:
  display_cols <- c("Visit", "Treatment", set, "N", "Miss", type_label,
                    "Prop", "OR (95% CI)", "p-value")
  attr(res, "display_cols") <- display_cols  
  
  ## Return
  res
}
# display_dttab(res)


#' Get OR, CI and p-value for an unconditional exact test using cache
#' 
#' CI computation takes a long time so this function builds and maintains a
#' simple cache saved at "05_cache/unconditional_exact_results_cache.csv"
#' to re-use previously computed results
#'
#' @param x1 
#' @param n1 
#' @param x2 
#' @param n2 
#'
#' @returns
#'
#' @examples
#' 
#' get_uce_test(33, 122, 39, 129)
#' get_uce_test(17, 145, 20, 145)
#' get_uce_test(18, 145, 28, 146)
#' get_uce_test(27, 145, 38, 146)
#' get_uce_test(33, 145, 39, 146)
#' get_uce_test(13, 145, 16, 145)
#' get_uce_test(16, 145, 21, 146)
#' get_uce_test(19, 145, 28, 146)
#' get_uce_test(26, 145, 32, 146)
#' get_uce_test(17, 145, 20, 145)
#' get_uce_test(8, 145, 15, 146)
#' get_uce_test(7, 145, 8, 146)
#' get_uce_test(5, 145, 4, 146)
#' 
get_uce_test <- function(x1, n1, x2, n2) {
  # x1 <- 33; n1 <- 122; x2 <- 39; n2 <- 129
  # x1 <- -1; n1 <- 122; x2 <- 39; n2 <- 129
  for(x in c(x1, n1, x2, n2)) # x <- x1
    stopifnot(is.numeric(x) && length(x) == 1 && 
                x >= 0 && abs(round(x) - x) < 1e-8)
  stopifnot(require("exact2x2", quietly = TRUE))

  FILE <- "05_cache/unconditional_exact_results_cache.csv"
  params <- c("estimate", "lwr_ci", "upr_ci", "p.value")
  uc_data <- data.table(x1, n1, x2, n2)
  uc_data[, xn_data := paste(x1, n1, x2, n2, sep = "-"), by = .I]
  
  FILE_exists <- file.exists(FILE)
  has_data <- FALSE
  if(FILE_exists) {
    bank <- fread(file = FILE, sep = ";")
    # bank[xn_data == uc_data$xn_data, .SD[1], .SDcols = params]
    has_data <- uc_data$xn_data %in% bank$xn_data
  }
  # FILE_exists && has_data   : read data
  # FILE_exists && !has_data  : make test and append to FILE
  # !FILE_exists              : make test and make FILE
  
  if(FILE_exists && has_data) {
    ans <- bank[xn_data == uc_data$xn_data, .SD[1], .SDcols = params]
    return(ans)
  }
  Test <- uncondExact2x2(x1=x1, n1=n1, x2=x2, n2=n2, 
                         parmtype = "oddsratio", 
                         conf.int = TRUE, 
                         tsmethod = "central")
  res <- Test[setDiff(names(Test), "conf.int")] |> as.data.table()
  res[, lwr_ci := Test$conf.int[1]]
  res[, upr_ci := Test$conf.int[2]]
  setcolorder(res, params)
  res <- cbind(uc_data, res)
  ans <- res[, .SD[1], .SDcols = params]
  if(FILE_exists) {
    bank <- fread(file = FILE, sep = ";")
    res <- rbind(bank, res)
  } 
  fwrite(res, file = FILE, sep = ";")
  
  # Return:
  ans[]
}


#' Compute binary Responder, Remission or Sustained Response Variable
#'
#' @param data 
#' @param type 
#' @param response 
#' @param penalty 
#' @param keep.baseline ignored unless `type == "binary"`
#'
#' @returns `data.table` with new variables `Y` (the binary response - `0`, `1`, 
#' or `NA`) and `cens` (binary censoring indicator - no `NA`s). 
#'
#' @examples
#' 
#' outc_var <- "madrs_total10"
#' dt <- get_outcome_data(outc_var, set = "FAS", estimand = "efficacy")
#' get_binary_outcome(dt, "Respon")[f_visit == "day28", .N, .(Y)]
#' get_binary_outcome(dt, "Remiss")[f_visit == "day28", .N, .(Y)]
#' get_binary_outcome(dt, "Sustai")[, .N, keyby = .(f_visit, Y)]
#' get_binary_outcome(dt, "Sustai")[f_visit == "day28", .N, .(Y)]
#' 
#' # Example with pre-defined binary outcome:
#' dt[, value2 := sample(0:1, size = .N, replace = TRUE)]
#' get_binary_outcome(dt, "binary", response = "value2")[, .N, .(Y)]
#' get_binary_outcome(dt, "binary", response = "value2", 
#'                    keep.baseline = TRUE)[, .N, .(Y)]
#' 
get_binary_outcome <- function(
    data, 
    type          = c("Responder", "Remission", "Sustained response", "binary"),
    response      = "value", 
    keep.baseline = FALSE,
    penalty       = 0) {
  stopifnot(is.data.table(data) && nrow(data) > 0L,
            is.character(response) && length(response) > 0L,
            is.logical(keep.baseline) && length(keep.baseline) == 1L,
            is.numeric(penalty) & length(penalty) == 1L)
  type <- match.arg(type) 
  # type <- "Responder"; response = "value"; data <- copy(dt); keep.baseline = FALSE
  # type <- "Sustained response"
  
  # Extract and order data:
  required_var <- c(response, "trt", "value0", "f_visit", "ID")
  test_required(required_var, names(data))
  .d <- data[, .SD, .SDcols = required_var]
  stopifnot("Data must contain one row per visit x ID" = 
              .d[, uniqueN(ID) * uniqueN(f_visit) == .N])

  # Remove baseline unless type == binary and keep.baseline == TRUE:  
  if(!(type == "binary" && keep.baseline)) 
    .d <- .d[!f_visit %in% "day0"]
  if(type == "Sustained response") 
    .d <- .d[f_visit %in% paste0("day", c(7, 14, 28))]
  setorder(.d, ID, f_visit)
  
  ## Derive response (Y) variable:
  .d[, .response := get(response) + penalty]
  .d[, Y := .response]           # type == "binary"
  if(type %in% c("Responder", "Sustained response"))
    .d[, Y := 1.0 * (.response < value0/2)]
  if(type == "Sustained response")
    .d[, Y := cumprod(Y), by=ID] # NA-values accumulate too
  if(type == "Remission")
    .d[, Y := 1.0 * (.response < 11)]
  
  ## Make censoring variable:
  .d[, cens := 1.0 * is.na(Y)]
  .d[]
}


#' Fit a Mixed Model for Repeated Measurements
#'
#' @param data 
#' @param visit 
#' @param response 
#' @param set 
#'
#' @returns
#'
#' @examples
#' 
#' outc_var <- "madrs_total10"
#' dt <- get_outcome_data(outc_var, set = "FAS", estimand = "efficacy")
#' dtpbl <- dt[n_target > 0] |> droplevels()
#' fit_mmrm(dtpbl, visit = "day7") |> display_dttab()
#' 
#' fit_mmrm(dtpbl, visit = "day14", response = "chgbl") |> display_dttab()
#' 
fit_mmrm <- function(data, 
                     # Use all visits up to 'visit'
                     visit = c("day7", "day14", "day28"),
                     response = "value",
                     set      = c("FAS", "SAS")) {
  stopifnot(is.data.table(data) && nrow(data) > 0,
            is.character(visit) && length(visit) == 1L,
            is.character(response) && length(response) > 0L)
  set <- match.arg(set)
  visit <- match.arg(visit)
  # data <- copy(dtpbl); response <- "value"; visit = "day7"; set <- "FAS"
  
  required_var <- 
    c("value0", "trt", "f_visit", "c_ID", "c_FAS", "c_SAS", response)
  test_required(required_var, names(data))
  .d <- data[, .SD, .SDcols = required_var]
  stopifnot("Data must contain one row per visit x ID" = 
              .d[, uniqueN(c_ID) * uniqueN(f_visit) == .N])
  
  # Determine visits:
  nvisit <- gsub("day", "", visit) |> as.numeric()
  valid_nvisit <- c(4, 7, 14, 28)
  use_visits <- paste0("day", valid_nvisit[valid_nvisit <= nvisit])

  # Get data for visits:
  vdata <- .d[f_visit %in% use_visits] |> droplevels()
  vdata[, cens := 1.0 * is.na(get(response))]
  
  # Identify treatment levels:  
  trt_levs <- get_trt_levs(vdata)
  ref_trt_lev <- trt_levs[1] |> unname()
  dexa_trt_lev <- trt_levs[2] |> unname()
  
  ## All formulae give identical models (but some provoke a note from emmeans):
  # form <- as.formula(paste0(response, " ~ f_visit + (value0 + trt):f_visit + us(f_visit | c_ID)"))
  # form <- as.formula(paste0(response, " ~ f_visit + value0/f_visit + trt/f_visit + us(f_visit | c_ID)"))
  # form <- as.formula(paste0(response, " ~ f_visit + value0:f_visit + trt:f_visit + us(f_visit | c_ID)"))
  # form <- as.formula(paste0(response, " ~ f_visit + (value0 + trt)*f_visit + us(f_visit | c_ID)"))
  form <- as.formula(paste0(response, " ~ (value0 + trt)*f_visit + us(f_visit | c_ID)"))
  
  ## These are different:
  # form <- as.formula(paste0(response, " ~ (value0 * trt)*f_visit + us(f_visit | c_ID)"))
  # form <- as.formula(paste0(response, " ~ (value0 * trt)/f_visit + us(f_visit | c_ID)"))
  model <- mmrm(form, data=vdata) 
  # coef(summary(model))
  # summary(model)
  
  ## Get [FS]AS, N, MISS, MEDIAN:
  tab_set <- if(set == "FAS") {
    .d[c_FAS == "yes", .(FAS = uniqueN(c_ID)), keyby=trt]
  } else {
    .d[c_SAS == "yes", .(SAS = uniqueN(c_ID)), keyby=trt]
  }
  tab_N <- vdata[, .(N = .N,
                     Miss = sum(cens == 1)), keyby=.(f_visit, trt)]
  tab_N <- merge(tab_N, tab_set, by="trt", all=TRUE, sort = FALSE)
  setorder(tab_N, f_visit, -trt)
  tab_N
  
  # Get emmeans:
  em <- emmeans(model, ~ trt | f_visit, data = vdata)
  df_em <- as.data.frame(em) |> as.data.table()
  df_em[, "Est. Mean (95% CI)" := 
          format_estci(emmean, lower.CL, upper.CL, dig=1, ci_sep = "; ")]
  df_em
  
  # Get treatment contrast:
  cem <- contrast(em, method = "trt.vs.ctrl", ref = ref_trt_lev, infer = TRUE) |> 
    as.data.frame() |> as.data.table()
  cem[, `:=`("Effect (95% CI)" = format_estci(estimate, lower.CL, upper.CL, dig=2, ci_sep = "; "),
             `p-value` = format_pval2(p.value),
             trt = dexa_trt_lev)]
  cem
  
  ## Impute missing values
  vdata[, pred_mmrm := predict(model, new_data=vdata)]
  vdata[, value_mmrm := fifelse(!is.na(get(response)), get(response), pred_mmrm)]
  
  ## Collect and return tables
  tab_emmeans <- merge(tab_N, df_em, by=c("f_visit", "trt"))
  nms <- c("SE", "df", "lower.CL", "upper.CL")
  setnames(tab_emmeans, nms, paste0(nms, ".emmean"))
  res <- merge(tab_emmeans, cem, by=c("f_visit", "trt"), all.x=TRUE)
  res[, Visit := gsub("day", "Day ", f_visit)]
  res[, Treatment := trt]
  setorder(res, f_visit, -trt)
  
  ## Save model and data as attributes  
  attr(res, "model") <- model
  attr(res, "data") <- vdata
  attr(res, "vcov_em") <- vcov(em, sep = " ")
  display_cols <- c("Visit", "Treatment", set, "N", "Miss",
                    "Est. Mean (95% CI)", "Effect (95% CI)", "p-value")
  attr(res, "display_cols") <- display_cols
  
  ## Return
  res
}


#' Fit subgroup MMRM
#'
#' @param data 
#' @param visits 
#' @param response 
#' @param subgroup_var 
#' @param subgroup_label 
#' @param set 
#'
#' @returns
#'
#' @examples
#' 
#' # Get data for MADRS10:
#' outc_var <- "madrs_total10"
#' dt0 <- get_outcome_data(outc_var, set = "FAS", estimand = "efficacy")
#' dbl <- readRDS(file = "02_data_derived/baselinedata.rds")
#' dt <- merge(dt0, dbl[, .(ID, sex, f_treatment_resistant, f_madrs10_30)], 
#'             by = "ID", all.x = TRUE)
#' 
#' # Fit subgroup model with default arguments:
#' fit_mmrm_subgroup(dt) |> display_dttab()
#' 
#' # Fit subgroup model to all visits until day 28:
#' fit_mmrm_subgroup(dt,
#'                   visit          = "day28",
#'                   subgroup_var   = "sex",
#'                   subgroup_label = "Sex",
#'                   set            = "FAS") |>
#'   display_dttab()
#' 
#' # Fit subgroup model until day 7:
#' fit_mmrm_subgroup(dt,
#'                   visit          = "day7",
#'                   subgroup_var   = "sex",
#'                   subgroup_label = "Sex",
#'                   set            = "FAS") |>
#'   display_dttab()
#' 
#' # Incorrect visit specification:
#' try(fit_mmrm_subgroup(dt, visit = "4-8"))  # expect error
#' try(fit_mmrm_subgroup(dt, visit = "day4")) # expect error
#' 
fit_mmrm_subgroup <- function(data, 
                              visit          = c("day7", "day14", "day28"),
                              response       = "value", 
                              subgroup_var   = "sex",
                              subgroup_label = "Sex",
                              set            = c("FAS", "SAS")) {
  # Check arguments:
  stopifnot(is.data.table(data) && nrow(data) > 0,
            is.character(response) && length(response) == 1L,
            is.character(subgroup_var) &&  length(subgroup_var) == 1L,
            is.character(subgroup_label) && length(subgroup_label) == 1L)
  set <- match.arg(set)
  visit <- match.arg(visit)
  # data <- copy(dt); visit <- "day7"; set <- "FAS"; response <- "chgbl"
  # subgroup_var = "f_treatment_resistant"; subgroup_label = "Treatment resistant"
  # subgroup_var = "f_madrs10_30"; subgroup_label <- "Severity" 
  
  required_var <- c(response, subgroup_var, "trt", "value0", "f_visit", 
                    "c_FAS", "c_SAS", "c_ID")
  test_required(required_var, names(data))
  .d <- data[, .SD, .SDcols = required_var]
  stopifnot("Data must contain one row per visit x ID" = 
              .d[, uniqueN(c_ID) * uniqueN(f_visit) == .N])
  
  # Determine visits:
  nvisit <- gsub("day", "", visit) |> as.numeric()
  valid_nvisit <- c(4, 7, 14, 28)
  use_visits <- paste0("day", valid_nvisit[valid_nvisit <= nvisit])

  .d[, Y := get(response)]
  if(.d[, is.factor(get(subgroup_var))]) {
    .d[, Group := get(subgroup_var)]
  } else {
    .d[, Group := factor(get(subgroup_var))]
  }
  .d[, cens := 1.0 * is.na(Y)]
  vdata <- .d[f_visit %in% use_visits] |> droplevels()
  
  # Identify treatment levels:  
  trt_levs <- get_trt_levs(vdata)
  ref_trt_lev <- trt_levs[1] |> unname()
  dexa_trt_lev <- trt_levs[2] |> unname()

  # Get [FS]AS, N, MISS, MEDIAN:
  tab_set <- if(set == "FAS") {
    .d[c_FAS == "yes", .(FAS = uniqueN(c_ID)), keyby = .(Group, trt)]
  } else {
    .d[c_SAS == "yes", .(SAS = uniqueN(c_ID)), keyby = .(Group, trt)]
  }
  tab_N <- vdata[, .(N = sum(cens == 0),
                     Miss = sum(cens == 1)), 
                 keyby=.(f_visit, Group, trt)]
  tab_N <- merge(tab_N, tab_set, by = c("Group", "trt"), all=TRUE, sort = FALSE)
  setkey(tab_N, Group, trt)
  tab_N

  # Fit model:
  form <- as.formula("Y ~ (value0 + trt * Group) * f_visit + us(f_visit | c_ID)")
  fm <- mmrm(form, data=vdata) 
  # coef(summary(fm))
  # dim(coef(summary(fm)))
  # summary(fm)
  
  # Compute EMMEANS:
  cov_reduce <- reformulate("Group", response = "value0")
  em <- emmeans(fm, "trt", by = c("f_visit", "Group"), data = vdata, 
                cov.reduce = cov_reduce)
  df_em <- as.data.frame(em) |> as.data.table()
  df_em[, "Est. Mean (95% CI)" := 
          format_estci(emmean, lower.CL, upper.CL, dig=1, ci_sep = "; ")]
  df_em
  
  ## Compute EFFECT:
  cem <- contrast(em, "trt.vs.ctrl", ref=ref_trt_lev, infer=TRUE) 
  het_test <- joint_tests(cem, by="f_visit") |> 
    as.data.frame() |> as.data.table()
  df_cem <- as.data.frame(cem) |> as.data.table()
  df_cem[, `:=`("Effect (95% CI)" = 
                  format_estci(estimate, lower.CL, upper.CL, dig=2, ci_sep = "; "),
                `p-value` = format_pval2(p.value),
                trt = dexa_trt_lev)]

  # Add interaction p-value (p-het):
  levs <- vdata[, levels(Group)]
  df_cem <- merge(df_cem, 
                  het_test[, .(Group=factor(last(levs), levels = levs), f_visit, 
                               `p-het` = format_pval2(p.value),
                               het.pvalue = p.value)], 
                  by=c("Group", "f_visit"), all.x=TRUE)
  df_cem

  # Collect and return tables:
  tab_emmeans <- merge(tab_N, df_em, by = c("f_visit", "trt", "Group"), all.x=TRUE)
  tab_emmeans <- cbind(subgroup_var = subgroup_var, tab_emmeans)
  nms <- c("SE", "df", "lower.CL", "upper.CL")
  setnames(tab_emmeans, nms, paste0(nms, ".emmean"))
  
  res <- merge(tab_emmeans, df_cem, by = c("f_visit", "trt", "Group"), all.x=TRUE)
  res[, Treatment := trt]
  res[, Visit := gsub("day", "Day ", f_visit)]
  levels(res$Group) <- gsub("<=", "\u2264", levels(res$Group))
  levels(res$Group) <- gsub(">=", "\u2265", levels(res$Group))
  setorder(res, f_visit, Group, -trt)
  setnames(res, "Group", subgroup_label)
  display_cols <- c("Visit", "Treatment", subgroup_label, set, "N", # "Miss", 
                    "Est. Mean (95% CI)", "Effect (95% CI)", "p-value", "p-het")
  attr(res, "display_cols") <- display_cols
  res
}

#' Compute IPC-weights
#'
#' @param vdata 
#' @param weight_model 
#' @param stabilized 
#'
#' @returns numeric vector weights of length `nrow(vdata)`
#'
#' @examples
#' 
#' # Get data for MADRS10:
#' outc_var <- "madrs_total10"
#' dt <- get_outcome_data(outc_var, set = "FAS", estimand = "policy")
#' 
#' # Get data for visit day 28:
#' vdata <- dt[f_visit == "day28"] |> droplevels()
#' vdata[, cens := 1.0 * is.na(value)]
#' 
#' # Some possible weights:
#' get_ipc_weights(vdata, weight_model = NULL) |> head()
#' get_ipc_weights(vdata, weight_model = "trt + value0") |> head()
#' get_ipc_weights(vdata, weight_model = "trt + value0", 
#'                 stabilized = FALSE) |> head()
#' 
get_ipc_weights <- function(vdata, weight_model, stabilized = TRUE) {
  stopifnot(is.data.table(vdata),
            "cens" %in% names(vdata) && vdata[, is.numeric(cens)],
            vdata[, sum(is.na(cens)) == 0 && all(cens %in% 0:1)],
            is.null(weight_model) || 
              (is.character(weight_model) && length(weight_model) == 1L),
            is.logical(stabilized) && length(stabilized) == 1L)
  
  # Return 1's if none are censored or weight_model is null:
  if(is.null(weight_model) || vdata[, sum(cens) == 0]) 
    return(rep(1.0, times = vdata[, .N]))
  
  ## Denominator:
  form_wt <- as.formula(paste0("cens ~ ", weight_model))
  # lr_denom <- glm(form_wt, data = vdata, family = binomial)
  lr_denom <- glm_quiet(form_wt, data = vdata, family = binomial)
  p_denom <- 1 - fitted(lr_denom) 
  stopifnot( all(is.finite(p_denom)) )
  
  ## Numerator:
  p_num <- 1.0
  if(stabilized) {
    stopifnot("trt" %in% names(vdata),
              vdata[, sum(is.na(trt)) == 0 && uniqueN(trt) == 2])
    lr_num <- glm(cens ~ trt, data = vdata, family = binomial)
    p_num <- 1 - fitted(lr_num)
    stopifnot( all(is.finite(p_num)) )
  }
  unname(p_num / p_denom) # IPC weights 
}

## helper function to silence warning about prior weights in emmeans:
emmeans_quiet <- function(...) {
  withCallingHandlers(
    emmeans::emmeans(...),
    warning = function(w) {
      if (grepl("prior weights", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

## helper function to silence warning about fitted probabilities in glm:
glm_quiet <- function(...) {
  withCallingHandlers(
    stats::glm(...),
    warning = function(w) {
      if (grepl("fitted probabilities numerically 0 or 1 occurred", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}


