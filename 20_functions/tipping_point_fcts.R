
#' Adjust a fit by a delta-shift
#' 
#' Convenience function that applies `delta_adjust_default()` to the results of
#' `fit`.
#'
#' @param fit a list with elements `mean_hat`, `vcov_hat`, and `df`
#' @param data a `data.table` with variables `cens` and `trt`
#' @param delta a length 2 numeric vector with shifts applied to reference and
#' active arms.
#'
#' @returns the same as `delta_adjust_defualt()`: a 3-row `data.table` of 
#' delta-adjusted estimated means and treatment
#' difference with 95% CI and p-value based on t-statistics.
#'
#' @examples
#' 
#' # Get data:
#' dt0 <- get_outcome_data("madrs_total10", set = "FAS", estimand = "policy")
#' dt0 <- dt0[n_target == 14]
#' dt0[, cens := 1.0 * is.na(value)]
#' trt_levs <- get_trt_levs(dt0)
#' dt0[, trt := relevel(trt, trt_levs[1])]
#' 
#' # Get IPCW fit to data:
#' fit <- get_ipcw_fit(dt0)
#' 
#' # Apply delta-adjustment to `fit`:
#' delta_adjust_fit(fit, dt0, delta = c(-2, 2))
#' 
#' # Implicitly estimate fit  before applying delta-adjustment:
#' delta_adjust_ipcw(dt0, delta = c(-2, 2))
#' 
delta_adjust_fit <- function(fit, data, delta = c(0, 0)) {
  stopifnot(is.list(fit),
            all(c("mean_hat", "vcov_hat", "df") %in% names(fit)),
            is.data.table(data),
            all(c("cens", "trt") %in% names(data)))
  delta_adjust_default(mean  = fit$mean_hat, 
                    vcov  = fit$vcov_hat, 
                    df    = mean(fit$df),
                    cens  = data[, cens], 
                    trt   = data[, trt],
                    delta = delta)
}

delta_adjust_ipcw <- function(data, delta = c(0, 0)) {
  # Arguments are checked in get_ipcw_fit() and delta_adjust_default()

  # Get IPCW fit to data:
  fit <- get_ipcw_fit(data)
  
  # Apply delta-adjustment and get results:
  delta_adjust_default(mean  = fit$mean_hat, 
                    vcov  = fit$vcov_hat, 
                    df    = fit$df,
                    cens  = data[, cens], 
                    trt   = data[, trt],
                    delta = delta)
} 


#' Apply delta-adjustment to estimated means
#'
#' @param mean estimated mean vector (numeric length 2)
#' @param vcov covariance matrix of the estimated means (numeric 2-by-2)
#' @param df degrees of freedom (numeric length 1 or 2)
#' @param cens censoring indicator data vector (numeric of length n)
#' @param trt treatment factor with 2 levels (factor of length n)
#' @param delta MNAR adjustment for reference and active arms (numeric length 2)
#'
#' @returns a 3-row `data.table` of delta-adjusted estimated means and treatment
#' difference with 95% CI and p-value based on t-statistics.
#'
#' @examples
#' 
#' # Get data:
#' dt0 <- get_outcome_data("madrs_total10", set = "FAS", estimand = "policy")
#' dt0 <- dt0[n_target == 14]
#' dt0[, cens := 1.0 * is.na(value)]
#' trt_levs <- get_trt_levs(dt0)
#' dt0[, trt := relevel(trt, trt_levs[1])]
#' 
#' # Get IPCW fit to data:
#' fit <- get_ipcw_fit(dt0)
#' 
#' # Apply delta-adjustment in active arm:
#' delta_adjust_default(fit$mean_hat, fit$vcov_hat, fit$df,
#'                      cens = dt0$cens, trt = dt0$trt, delta = c(0, 2))
#' 
delta_adjust_default <- function(mean, vcov, df = Inf, cens, trt, delta = c(0, 0)) {
  # mean <- mar$mean_hat; vcov <- mar$vcov_hat; df <- mar$df; 
  # cens <- dt0$cens; trt <- dt0$trt; delta <- c(0, 0)
  stopifnot(is.numeric(mean) && length(mean) == 2,
            is.numeric(vcov) && is.matrix(vcov) && all(dim(vcov) == c(2, 2)),
            is.numeric(cens) && all(cens %in% 0:1),
            is.factor(trt) && nlevels(trt) == 2,
            length(cens) == length(trt),
            is.numeric(df) && length(df) >= 1 && all(df >= 1),
            is.numeric(delta) && length(delta) == 2)
  
  trt_levs <- levels(trt)
  .d <- data.table(cens, trt)
  
  # Proportion of missing values by treatment group:
  tab <- .d[, .(N = .N, Nmiss = sum(cens == 1)), 
            keyby = trt]
  setnames(tab, "trt", "coef")
  tab[, pmiss := Nmiss / N]
  tab[, delta := delta]
  tab[, mu_adj  := pmiss * delta]
  tab[, var_adj := pmiss * (1 - pmiss) * delta^2 / N]
  tab
  
  # Adjust mean and vcov due to delta:
  mean_adj <- mean + tab[, mu_adj]
  vcov_adj <- vcov + diag(tab[, var_adj])
  
  # Estimated treatment difference (ETD):
  L <- c(-1, 1) # contrast vector
  etd <- L %*% mean_adj |> drop() 
  vcov_etd <- t(L) %*% vcov_adj %*% L |> drop()
  
  # Results table:
  res <- rbind(
    data.table(coef = factor(trt_levs, levels = trt_levs), 
               est = mean, SE = sqrt(diag(vcov)), df = df),
    data.table(coef = "ETD", est = etd, SE = sqrt(vcov_etd), df = mean(df)))
  res[, lwr := est + qt(0.025, df = df) * SE]
  res[, upr := est + qt(0.975, df = df) * SE]
  res[, pval := 2 * pt(abs(est / SE), df = df, lower.tail = FALSE)]
  res[, `p-value` := format_pval2(pval)]
  
  merge(tab, res, by = "coef", sort = FALSE, all = TRUE)
}


#' Extract emmeans vector and its covariance matrix from a MAR fit
#' 
#' Requires that the pipeline has been run to produce the relevant results.
#'
#' @param regi  a one-row metadata `data.table` originating from `get_metadata()`
#'
#' @returns a length-3 named list with elements 
#' 
#' - `mean_hat` the estimated mean vector
#' - `df` denominator degrees of freedom and 
#' - `vcov_hat` covariance matrix for the estimated mean vector
#'
#' @examples
#' 
#' # Get metadata:
#' tlf_reg  <- get_metadata()
#' regi_all <- tlf_reg[chapter == "madrs_total10" & 
#'                       outcome_kind == "chgbl" & 
#'                       role == "Primary" & 
#'                       tlf_type == "table" & 
#'                       estimand == "policy"]
#' 
#' # ANCOVA-fit:                       
#' regi <- regi_all[1]
#' # get_mar_fit(regi) # requires pipeline has been run
#' 
#' # MMRM-fit:
#' # get_mar_fit(regi_all[2]) # requires pipeline has been run
#' 
get_mar_fit <- function(regi) { # get_mmrm_fit
  stopifnot(is.data.table(regi),
            nrow(regi) == 1L,
            regi[, tlf_type == "table"],
            regi[, analysis %in% c("ancova", "mmrm", "t-test")])

  # Extract emmeans and df:  
  n_visit <- regi[, visit]
  c_visit <- paste("Day", n_visit)
  tab <- get_dt_table(regi)
  mean_hat <- tab[Visit == c_visit, emmean]
  df <- tab[Visit == c_visit, df.emmean]

  # Extract vcov(emmeans):  
  if(!"vcov_em" %in% names(attributes(tab))) 
    stop("'vcov_em' is not saved as an attribute on the analysis table")
  if(regi[, analysis == "mmrm"]) {
    vcov_em <- attr(tab, "vcov_em")
    ind <- getgrep(paste0("day", n_visit), colnames(vcov_em))
    vcov_hat <- vcov_em[ind, ind]
  } else {
    vcov_hat <- attr(tab, "vcov_em")
  }

  namedList(mean_hat, df, vcov_hat)
}


#' Get ANCOVA-IPCW fit from data
#' 
#' Fit an inverse probability of censoring weighted linear model to data.
#'
#' @param data data a `data.table` with variables `value0` `cens`, `trt`, 
#' and `value`
#'
#' @returns a list with elements `model`, `mean_hat`, `df`, `vcov_hat` 
#' and `m_value0`
#'
#' @examples
#' 
#' # Get data:
#' dt0 <- get_outcome_data("madrs_total10", set = "FAS", estimand = "policy")
#' dt0 <- dt0[n_target == 14]
#' dt0[, cens := 1.0 * is.na(value)]
#' trt_levs <- get_trt_levs(dt0)
#' dt0[, trt := relevel(trt, trt_levs[1])]
#' 
#' # Get IPCW fit to data:
#' get_ipcw_fit(dt0)
#' 
get_ipcw_fit <- function(data) {
  response <- "chgbl"
  stopifnot(is.data.table(data) && nrow(data) > 1)
  required_cols <- c("value0", "cens", "trt", "value")
  test_required(required_cols, names(data), name = "data")
  .d <- data[, .SD, .SDcols = required_cols]
  stopifnot(.d[, is.factor(trt)],
            sum(!complete.cases(.d[, .(trt, cens, value0)])) == 0)
  trt_levs <- get_trt_levs(data) |> unname()
  .d[, chgbl := value - value0]
  .d[, Y := get(response)]
  
  # Fit IPC-weighted ANCOVA:
  lr_den <- glm(cens ~ value0 + trt, data = .d, family = binomial) |> 
    suppressWarnings()
  lr_num <- glm(cens ~ trt, data = .d, family = binomial) |> 
    suppressWarnings()
  .d[, swt := (1 - fitted(lr_num)) / (1 - fitted(lr_den))]
  model <- lm(Y ~ 1 + value0 + trt, data = .d[cens == 0], weights = swt)
  
  # Extract from model:
  beta_hat <- coef(model)
  X <- model.matrix(model)
  df <- nrow(X) - sum(!is.na(beta_hat))
  Vcov <- vcov(model)
  
  # Extract from data:
  m_value0 <- .d[, mean(value0, na.rm =TRUE)]
  
  # Estimated means:
  ndata <- expand.grid(value0 = m_value0, 
                       trt    = trt_levs)
  Xnew <- model.matrix(~ 1 + value0 + trt, data = ndata)
  rownames(Xnew) <- trt_levs
  stopifnot(all(colnames(Xnew) == names(beta_hat)))
  mean_hat <- Xnew %*% beta_hat |> drop()
  
  # Covariance matrix of the estimated means:
  vcov_hat <- Xnew %*% Vcov %*% t(Xnew)
  
  namedList(model, mean_hat, df, vcov_hat, m_value0)
}


# data <- copy(dt28); response <- "chgbl"; delta_max <- 20; arm <- dexa_trt_lev
# data <- copy(dt28b); response <- "chgbl"; delta_max <- 20; 
#    arm <- c(dexa_trt_lev, ref_trt_lev)


#' Estimate one-way or two-way tipping points 
#' 
#' The _tipping point_ is the value of the delta-adjustment required to tip the 
#' estimated treatment difference (ETD) from significant to non-significant.
#' 
#' In the two-way situation the tipping point value being estimated is 
#' identified by requiring the delta-adjustment to be of equal magnitude but in
#' different directions in reference and active arms.
#' 
#' Numerical root-finding is used to calculate the tipping point value.
#' 
#' The tipping point value is not estimated if there are no missing values, if 
#' the ETD is not significant without adjustments, or if a mean shift larger 
#' than `delta_max` is required to tip the ETD to non-significance.
#'
#' @param data a `data.table` with variables `cens` and `trt`, `value0`, and 
#' `value`
#' @param arm a length 1 (one-way) or length 2 (two-way) vector of treatment 
#' levels indication which level(s) to estimate the tipping point for
#' @param fit a list with elements `mean_hat`, `df`, and `vcov_hat`. 
#' @param delta_max the maximum delta-adjustment to consider for tipping point 
#' estimation
#'
#' @returns a `data.table` with a summary of the tipping point estimation.
#'
#' @examples
#' 
#' # Get data:
#' dt0 <- get_outcome_data("madrs_total10", set = "FAS", estimand = "policy")
#' dt0 <- dt0[n_target == 14]
#' dt0[, cens := 1.0 * is.na(value)]
#' trt_levs <- get_trt_levs(dt0)
#' dt0[, trt := relevel(trt, trt_levs[1])]
#' 
#' # Get IPCW fit to data:
#' fit <- get_ipcw_fit(dt0)
#' 
#' # One-way TP analysis with adjustment in the active arm:
#' tipping_point(dt0, trt_levs[2], fit)
#' 
#' # Two-way TP analysis with adjustment in both arms:
#' tipping_point(dt0, trt_levs, fit)
#' 
tipping_point <- function(data, arm, fit = NULL, delta_max = 20) {
  response <- "chgbl" 
  stopifnot(is.data.table(data),
            response %in% c("value", "chgbl") && length(response) == 1,
            is.numeric(delta_max) && length(delta_max) == 1,
            is.character(arm) && length(arm) %in% 1:2)
  required_cols <- c("value0", "cens", "trt", "value")
  test_required(required_cols, names(data), name = "data")
  .d <- data[, .SD, .SDcols = required_cols]
  trt_levs <- get_trt_levs(data)
  if(response == "chgbl") .d[, chgbl := value - value0]
  .d[, Y := get(response)]
  
  # require that trt is a factor with level 'trt_levs' 
  #   and 'arm' are levels in trt
  # require no missing values for 'trt' and 'cens' variables.
  stopifnot(.d[, is.factor(trt)],
            all(.d[, levels(trt)] == trt_levs),
            all(arm %in% trt_levs),
            sum(!complete.cases(.d[, .(trt, cens)])) == 0)
  
  make_res_tab <- function(res0, TP) {
    tab <- res0[, .(coef, N, pmiss, MAR = est)]
    tab[coef != "ETD", penalty := fifelse(coef %in% arm, "Yes", "No")]
    tab[penalty == "Yes" | coef == "ETD", TP := TP]
    if(is.numeric(TP)) {
      tab[, pct_diff := TP / abs(MAR) * 100]
      # % of ETD in *both* directions for two-way TP, so multiply by length(arm):
      tab[coef == "ETD", pct_diff := pct_diff * length(arm)] 
      tab[, pct_diff := do_format(pct_diff, digits = 1)] 
      tab[, TP := do_format(TP, digits = 1)]
    } else tab[, pct_diff := NA_character_]
    setattr(tab, "delta_results", res0)
    tab[]
  }
  
  # Check that arm(s) contain missing values:
  miss_tab <- .d[, .(nmiss = sum(cens)), keyby = trt]
  any_censored <- miss_tab[trt %in% arm, any(nmiss > 0)]
  
  # Get IPCW fit to data:
  if(is.null(fit)) fit <- get_ipcw_fit(.d)
  stopifnot(is.list(fit) && 
              all(c("mean_hat", "vcov_hat", "df") %in% names(fit)))

  # MAR results:
  res0 <- delta_adjust_fit(fit, .d, delta = c(0, 0))

  if(!any_censored) return(make_res_tab(res0, TP = "No missing values"))

  # Check that ETD is significant:
  etd_ns <- res0[coef == "ETD", upr > 0]
  if(etd_ns) return(make_res_tab(res0, TP = "Effect is not significant"))
  
  # Check if ETD is non-significant at delta_max:
  if(trt_levs[2] %in% arm && # Active
     miss_tab[trt == trt_levs[2], nmiss > 0]) { # any censored values for this arm
    res_a <- delta_adjust_fit(fit, .d, delta = c(0, delta_max))
    act_out_of_range <- res_a[coef == "ETD", upr < 0]
    if(act_out_of_range) {
      ans <- paste0("Tipping Point > ", 
                    CRP::do_format(delta_max))
      return(make_res_tab(res0, TP = ans))
    }
  }
  if(trt_levs[1] %in% arm && # Placebo/Reference
     miss_tab[trt == trt_levs[1], nmiss > 0]) { # any censored values for this arm
    res_r <- delta_adjust_fit(fit, data = .d, delta = c(-delta_max, 0))
    ref_out_of_range <- res_r[coef == "ETD", upr < 0]
    if(ref_out_of_range) {
      ans <- paste0("Tipping Point > ", CRP::do_format(delta_max))
      return(make_res_tab(res0, TP = ans))
    }
  }

  # Find root:
  root_fun <- function(del, arm, fit, data) { # del <- 5
    Delta <- c(0, 0)
    if(trt_levs[1] %in% arm) Delta[1] <- -del
    if(trt_levs[2] %in% arm) Delta[2] <- del
    
    res <- delta_adjust_fit(fit, data, delta = Delta)
    res[coef == "ETD", upr]
  }
  
  root_res <- uniroot(f = root_fun, interval = c(0, delta_max), arm = arm,
                      fit = fit, data = .d)
  tol_uniroot <- 1e-3
  converged <- abs(root_res$f.root) < tol_uniroot
  if(!converged) 
    stop("Estimation of tipping point failed:\n", 
         "  uniroot() did not converge: f(root) = ", 
         format(root_res$f.root, scientific = TRUE, digits = 2), " > ", 
         format(tol_uniroot, scientific = TRUE, digits = 2))
  TP <- root_res$root

  make_res_tab(res0, TP)
}

# delta_vec = seq(-20, 20, by = 1); data <- copy(.d); ylab_sub = trt_levs
# delta_vec = seq(-20, 20, by = 1); data <- copy(dt28b); ylab_sub = trt_levs

#' One-way tipping point plot
#' 
#' Plot a series of estimated treatment differences (ETDs) with 
#' associated 95% confidence intervals for each treatment arm using `ggplot2`.
#'
#' @param fit a list with elements `mean_hat`, `df`, and `vcov_hat`. 
#' @param data a `data.table` with variables `cens` and `trt`, `value0`, and 
#' `value` 
#' @param plot produce the plot (`TRUE`) or return the `data.table` on which the 
#' plot is based (`FALSE`)
#' @param ylab_sub length 2 character vector of labels for reference and active
#' treatment arms 
#' @param delta_vec values of delta for which to obtain and plot delta-adjusted 
#' ETD and 95% CI (numeric vector)
#'
#' @returns if `plot = FALSE` a `data.table` and if `plot = TRUE` the plot is
#' produced
#'
#' @examples
#' 
#' # Get data:
#' dt0 <- get_outcome_data("madrs_total10", set = "FAS", estimand = "policy")
#' dt0 <- dt0[n_target == 14]
#' dt0[, cens := 1.0 * is.na(value)]
#' trt_levs <- get_trt_levs(dt0)
#' dt0[, trt := relevel(trt, trt_levs[1])]
#' 
#' # Get IPCW fit to data:
#' fit <- get_ipcw_fit(dt0)
#' 
#' # The one-way tipping point plot:
#' tp_one_way(fit, dt0, ylab_sub = trt_levs)
#' 
tp_one_way <- function(fit, data, plot = TRUE,
                       ylab_sub = c("reference", "active"),
                       delta_vec = seq(-20, 20, by = 1)) {
  stopifnot(is.list(fit),
            all(c("mean_hat", "vcov_hat", "df") %in% names(fit)),
            is.data.table(data),
            all(c("cens", "trt") %in% names(data)),
            is.logical(plot) && length(plot) == 1,
            is.character(ylab_sub) && length(unique(ylab_sub)) == 2,
            is.numeric(delta_vec) && length(delta_vec) > 1 &&
              all(is.finite(delta_vec)))
  
  ddel <- lapply(ylab_sub, function(arm) {
    lapply(delta_vec, function(del) {
      Delta <- if(arm == ylab_sub[1]) c(del, 0) else c(0, del)
      res <- delta_adjust_fit(fit, data = data, delta = Delta)
      cbind(arm = arm, del_dex = del, res)
    }) |> rbindlist()
  }) |> rbindlist()
  ddel[, arm := factor(arm, levels = ylab_sub)]
  ddel[, conservatism := fifelse(arm == ylab_sub[2], del_dex, -del_dex)]
  max_abs <- max(abs(delta_vec))
  
  if(!plot) return(ddel)
  
  ggplot(ddel[coef == "ETD"], aes(est, del_dex, color = conservatism)) + 
    theme_bw() + theme(legend.position = "right") + 
    geom_point() + 
    geom_segment(aes(x = lwr, xend = upr), linewidth = 1) + 
    geom_vline(aes(xintercept = 0), linetype = "dashed") + 
    scale_color_gradient2(
      low = "#E69F00", mid = "grey70", high = "#0072B2",
      midpoint = 0,
      limits = c(-max_abs, max_abs),
      breaks = c(-max_abs, 0, max_abs),
      labels = c("Favorable", "MAR (\u03b4=0)", "Conservative"),
      name = "Direction of\nadjustment"
    ) + 
    xlab("Estimated Treatment Difference") + 
    ylab(bquote(delta)) + 
    facet_wrap(~arm)
}

# delta_ref = seq(-5, 5, by = .5); delta_act = seq(-5, 5, by = .5)
# data <- copy(dt28b); plot <- TRUE; ylab_sub = c("reference", "active")
# data <- copy(.d)

#' Two-way tipping point contour plot
#' 
#' Make a contour plot of the upper 95% confidence limit for the estimated 
#' treatment differences (ETDs). The solid black contour identifies the tipping
#' point curve on a grid spanned by delta in reference and active directions.
#' 
#' `ggplot2` and `geom_contour()` is used for plotting.
#'
#' @param fit a list with elements `mean_hat`, `df`, and `vcov_hat`. 
#' @param data a `data.table` with variables `cens` and `trt`, `value0`, and 
#' `value` 
#' @param plot produce the plot (`TRUE`) or return the `data.table` on which the 
#' plot is based (`FALSE`)
#' @param ylab_sub length 2 character vector of labels for reference and active
#' treatment arms 
#' @param delta_vec values of delta for which to obtain and plot delta-adjusted 
#' ETD and 95% CI (numeric vector)
#' @param delta_ref values of delta for the reference arm (numeric vector)
#' @param delta_act values of delta for the active arm (numeric vector)
#'
#' @returns if `plot = FALSE` a `data.table` and if `plot = TRUE` the plot is
#' produced
#'
#' @examples
#' 
#' # Get data:
#' dt0 <- get_outcome_data("madrs_total10", set = "FAS", estimand = "policy")
#' dt0 <- dt0[n_target == 14]
#' dt0[, cens := 1.0 * is.na(value)]
#' trt_levs <- get_trt_levs(dt0)
#' dt0[, trt := relevel(trt, trt_levs[1])]
#' 
#' # Get IPCW fit to data:
#' fit <- get_ipcw_fit(dt0)
#' 
#' # The two-way tipping point contour plot:
#' tp_two_way(fit, dt0, ylab_sub = trt_levs)
#' 
tp_two_way <- function(fit, data, plot = TRUE,
                       ylab_sub = c("reference", "active"),
                       delta_ref = seq(-5, 5, by = .5), 
                       delta_act = seq(-5, 5, by = .5)) {
  stopifnot(is.list(fit),
            all(c("mean_hat", "vcov_hat", "df") %in% names(fit)),
            is.data.table(data),
            all(c("cens", "trt") %in% names(data)),
            is.logical(plot) && length(plot) == 1,
            is.numeric(delta_ref) && length(delta_ref) > 1 &&
              all(is.finite(delta_ref)),
            is.numeric(delta_ref) && length(delta_ref) > 1 &&
              all(is.finite(delta_ref)))
  
  # Compute delta penalties:
  del_mat <- expand.grid(ref = delta_ref, act = delta_act) |> as.data.table()
  ddel <- lapply(seq_len(del_mat[, .N]), function(i) { # i <- 1
    res <- delta_adjust_fit(fit, data=data, delta = del_mat[i, c(ref, act)])
    cbind(del_mat[i], res) 
  }) |> rbindlist()
  
  if(!plot) return(ddel)
  
  rref <- range(delta_ref)
  ract <- range(delta_act)
  crosses_zero <- ddel[coef == "ETD", prod(range(upr)) < 0]
  min_upr <- ddel[coef == "ETD", min(upr)]
  max_upr <- ddel[coef == "ETD", max(upr)]
  mid_upr <- ddel[coef == "ETD" & ref == min(abs(ref)) & 
                    act == min(abs(act))][1, upr]

  nice_breaks <- function(x, n = 10) {
    brks <- pretty(range(x, na.rm = TRUE), n = n)
    # figure out how many decimal places the step size implies,
    # and round to well beyond that to remove floating-point deviations
    step <- diff(brks)[1]
    decimals <- max(0, -floor(log10(step)) + 6)
    round(brks, decimals)
  }
  my_breaks <- nice_breaks(ddel[coef == "ETD"]$upr)

  # Contour plot:
  p <- ggplot(ddel[coef == "ETD"], aes(x=ref, y = act)) + 
    theme_classic() + theme(panel.background=element_blank(),
                            legend.position = "right") + 
    coord_fixed() + 
    geom_raster(aes(fill = upr)) + 
    geom_contour(aes(z = upr), breaks = my_breaks, colour = "white", linewidth=0.2, alpha=0.8 )
  if(crosses_zero) p <- p + 
    geom_contour(aes(z = upr), colour = "black", breaks = 0, linewidth = 1.2)
  p <- p + 
    annotate("segment", x = 0, y = ract[1], yend = ract[2], linetype = "dashed") +
    annotate("segment", x = rref[1], xend = rref[2], y = 0, linetype = "dashed") +
    geom_text_contour(aes(z = upr), colour="white", breaks = my_breaks,
                      label.placer = label_placer_fraction(frac = 0.5),
                      skip = 0, size = 5, fontface="bold") +
    xlab(bquote(delta[.(ylab_sub[1])])) + 
    ylab(bquote(delta[.(ylab_sub[2])])) + 
    scale_x_continuous(breaks = rref[1]:rref[2], minor_breaks = NULL) +
    scale_y_continuous(breaks = ract[1]:ract[2], minor_breaks = NULL)
  if(!isTRUE(all.equal(min_upr, max_upr)))
    p <- p + scale_fill_gradient2(
      low = "#E69F00", mid = "grey80", high = "#0072B2",
      midpoint = mid_upr,
      limits = c(min_upr, max_upr),
      breaks = c(min_upr, max_upr),
      labels = c("Favorable", "Conservative"),
      name = "Direction of\nadjustment"
    )
  
  p
}
