

#' Make dataset with visits, target dates and visit windows for all IDs
#' 
#' @description The visits target dates and window sizes are defined as 
#' follows:
#' 
#'   visits <- c("day0", "day4", "day7", "day14", "day28", "month6")
#'   target <- c(    0,      4,      7,      14,      28,      182)
#'   window <- c(    0,      2,      2,       2,       4,       28) 
#' 
#' @param dt_day0 `data.table` object with columns `ID` and `d_day0`
#' @param today A `Date` object of length `1`. Defaults to the date of the 
#' RedCap database data extraction.
#'
#' @returns A `data.table` with `uniqueN(ID)` by `6` rows: one row for each 
#' combination of ID and visit. Columns include:
#' `c("ID", "c_visit", "n_target", "d_target", "d_lwr", "d_upr", "d_today")`
#'
#' @examples
#' 
#' mk_target_dates()
#' 
mk_target_dates <- function(
    dt_day0 = readRDS("02_data_derived/day0_all_subjects.rds"), 
    today = get_db_date()) {
  # Check inputs:
  stopifnot(is.data.table(dt_day0) && nrow(dt_day0) > 0,
            is.Date(today) && length(today) == 1L)
  required_cols <- c("ID", "d_day0")
  test_required(required_cols, names(dt_day0), "dt_day0")
  .d <- copy(dt_day0)[,  .SD, .SDcols = required_cols] |> unique()
  
  # Generate all visits:
  visits <- c("day0", "day4", "day7", "day14", "day28", "month6")
  target <- c(0, 4, 7, 14, 28, 182)
  window <- c(0, 2, 2,  2,  4,  28) 
  # FIXME: operate in terms of f_visit instead?
  dt_visits <- data.table(c_visit  = visits, 
                          n_target = target, 
                          window   = window)
  
  # Make dataset with all ID x visits with d_day0
  dt_target <- .d[, .(c_visit = dt_visits$c_visit), .(ID, d_day0)]
  
  # Add d_target, visit window (d_lwr and d_upr) and d_today:
  dt_target <- dt_target[dt_visits, on="c_visit"] # Add n_target & window
  dt_target[, `:=`(d_target = d_day0 + n_target,
                   d_lwr = d_day0 + n_target - window,
                   d_upr = d_day0 + n_target + window,
                   d_today = as.IDate(today))]
  
  dt_target[]
}


#' Compute visit window information
#' 
#' @description
#' For each observation (identified by `c("ID", "date", "c_visit")`) 
#' compute 
#' 
#' 1. `n_days_from_target` 
#' 2. The "window `state`" classifying 
#'    a. missing observations as "late", "now" or "future"
#'    b. observed observation as "in_window" or "outside_window"
#' 
#'
#' @param data data with observed visits. `data.table` objects with required 
#' columns `c("ID", "date", "c_visit")`
#' @param dt_target optional `data.table` the result of a call to 
#' `mk_target_dates()`
#' @param dt_day0 optional `data.table` object with day0 dates for all IDs
#'
#' @returns A `data.table` with at least one row per combination of ID
#' and visit corresponding to all potential and actual observation dates. 
#' If there are multiple dates per visit for any ID, all these dates are
#' included in the returned `data.table`.
#'
#' @examples
#' 
#' # Get "example" data for MADRS10:
#' lst <- readRDS("02_data_derived/data_by_category.rds")
#' tmp <- rbindlist(lst, fill=TRUE)
#' tmp1 <- tmp[!is.na(rand_id) & variable == "madrs" & part == "total10"]
#' 
#' # Get windows data for all realized and potential dates:
#' dwin <- get_windows(tmp1)
#' dwin[]
#' # Note that there are more rows than in dt_target
#' dt_target <- mk_target_dates()
#' stopifnot(dwin[, .N] >= dt_target[, .N])
#' 
#' # If `rand_id` is available and has NAs a warning is issued:
#' tmp2 <- tmp[variable == "madrs" & part == "total10"]
#' get_windows(tmp2) # warning: "Cases of `NA` in `rand_id` in data"
#' get_windows(tmp2[!is.na(rand_id)]) # no warning
#' 
get_windows <- function(
    data, 
    dt_target = NULL, 
    dt_day0 = readRDS("02_data_derived/day0_all_subjects.rds")) {
  # Check inputs:
  stopifnot(is.data.table(data) && nrow(data) > 0)
  required_cols <- c("ID", "date", "c_visit")
  valid_visits <- c("day0", "day4", "day7", "day14", "day28", "month6")
  test_required(required_cols, names(data), "data")
  stopifnot(is.numeric(data[, ID]),
            is.Date(data[, date]),
            is.character(data[, c_visit]))
  if("rand_id" %in% names(data) && data[, any(is.na(rand_id))])
    warning("Cases of `NA` in `rand_id` in data")
  n_invalid_visits <- data[!c_visit %in% valid_visits, .N]
  if(n_invalid_visits > 0)
    warning("Dropping ", n_invalid_visits, " cases of invalid visits")
  .d <- copy(data)[c_visit %in% valid_visits, .SD, .SDcols = required_cols] |>
    unique() |> droplevels()
  
  # Get target dates if not supplied:  
  if(is.null(dt_target)) {
    dt_target <- mk_target_dates(dt_day0)
  }
  required_target_cols <- c("ID", "d_day0", "c_visit", "n_target", "window", 
                            "d_target", "d_lwr", "d_upr", "d_today")
  test_required(required_target_cols, names(dt_target), "dt_target")
  
  # Include all possible assessment dates: .d[, date] and dt_taget[, d_target] 
  dwin <- merge(.d, dt_target, by=c("ID", "c_visit"),
                all = TRUE, sort = FALSE)
  
  # Add days from target (negative before target date, positive after target date)
  dwin[, n_days_from_target := fifelse(is.na(date), as.numeric(d_today - d_target),
                                       as.numeric(date - d_target))]
  
  # Mark missing observations as late, now or future:
  dwin[is.na(date) & d_upr < d_today, state := "late"]
  dwin[is.na(date) & d_lwr <= d_today & d_today <= d_upr, state := "now"]
  dwin[is.na(date) & d_today < d_lwr, state := "future"]
  
  # Mark recorded observations as in- or outside visit windows:
  dwin[!is.na(date), state := 
         fifelse(d_lwr <= date & date <= d_upr, "in_window", "outside_window")]
  
  dwin[]
}


#' Get unique visit dates
#' 
#' @description
#' Select unique observations for each ID by visit combination:
#' 
#' If there are multiple observations for each ID x visit with a non-`NA` date,
#' choose the one closest to the target date and break ties by selecting the
#' earliest date to pick observations on-treatment if possible.
#' 
#'
#' @param data data with observed visits. `data.table` objects with required 
#' columns `c("ID", "date", "c_visit")`
#' @param dontrt `data.table` with on-treatment information; required columns 
#' are `c("ID", "d_on_treat", "i_on_treat")`
#' @param dt_day0 `data.table` with day0 dates for all IDs
#'
#' @returns A `data.table` with exactly one row per combination of ID
#' and visit
#'
#' @examples
#' 
#' # Get "example" data for MADRS10:
#' lst <- readRDS("02_data_derived/data_by_category.rds")
#' tmp <- rbindlist(lst, fill=TRUE)
#' tmp1 <- tmp[!is.na(rand_id) & variable == "madrs" & part == "total10"]
#' 
#' # Get unique visit data for MADRS10:
#' get_unique_visit_data(tmp1)
#' 
get_unique_visit_data <- function(
    data, 
    dontrt  = readRDS("02_data_derived/treatment_compliance_all_rand_id.rds"),
    dt_day0 = readRDS("02_data_derived/day0_all_subjects.rds")) { # data <- copy(tmp1)
  # Check arguments:
  stopifnot(is.data.table(data) && nrow(data) > 0)
  required_cols <- c("ID", "date", "c_visit")
  test_required(required_cols, names(data), "data")
  .d <- copy(data)[, .SD, .SDcols = required_cols]
  
  # Get unique visits with non-NA data:
  unique_visits <- .d[, unique(c_visit)]
  dt_target <- mk_target_dates(dt_day0)
  # Make data with 1 row per ID x c_visit (for unique_visits):
  dt_target <- dt_target[c_visit %in% unique_visits]

  # Add "i_on_treat" variable to data:
  require_ontrt_cols <- c("ID", "d_on_treat", "i_on_treat")
  test_required(require_ontrt_cols, names(dontrt), "dontrt")
  .d <- merge(.d, dontrt[, .SD, .SDcols = require_ontrt_cols], 
              by.x = c("ID", "date"), by.y = c("ID", "d_on_treat"), 
              all.x = TRUE, sort = FALSE)
  
  # Get windows data:
  .dwin <- get_windows(data = .d, dt_target = dt_target)
  .dwin_realized <- .dwin[!is.na(date), .(ID, c_visit, date, n_days_from_target)]
  
  # Add 'n_days_from_target' to raw (realized visits) data:
  .d <- merge(.d, .dwin_realized, by = c("ID", "c_visit", "date"), all.x = TRUE)
  
  # Select unique observations over ID x c_visit:
  ## If there are multiple observations for each ID x visit with a non-NA date,
  ## choose the one closest to the target date and break ties by selecting the
  ## earliest date to pick observations on-treatment if possible:
  .d[, n_abs_days_from_target := as.integer(abs(n_days_from_target))]
  setorder(.d, ID, c_visit, n_abs_days_from_target, -i_on_treat, date)
  .d <- .d[, .SD[1], .(ID, c_visit)]
  
  ## Check that there is exactly one observation per ID x c_visit:
  stopifnot(.d[, .N, .(ID, c_visit)][, all(N == 1)])
  
  # Make data on all realized and unrealized visits:
  .d <- merge(dt_target[, .(ID, c_visit, d_day0, n_target, d_target, 
                            d_lwr, d_upr, d_today)], 
              .d[, .(ID, c_visit, date, i_on_treat)], 
              by = c("ID", "c_visit"), all.x = TRUE)
  
  # Add window-info for all realized and unrealized visits:
  .d <- merge(.d, .dwin[, .(ID, c_visit, date, n_days_from_target, state)], 
              by = c("ID", "c_visit", "date"),
              all.x = TRUE, sort = FALSE)
  
  .d[]
}


#'  Classify observations according to missing status
#'  
#'  @description
#'  Missing status is defined as
#'    - obs. inside the visit window are "in_window"
#'    - obs. that are not in window are "dropout" if all later observations 
#'      are not in visit window; otherwise they are "intermittent"
#'    - obs. to be recorded now or in the future are, however, "unknown"
#'
#' @param data `data.table` with columns `c("ID", "n_target", "state")`
#'
#' @returns `data.table` with columns `c("ID", "n_target", "state", "missing")`
#'
#' @examples
#' 
#' # Get "example" data for MADRS10:
#' lst <- readRDS("02_data_derived/data_by_category.rds")
#' dontrt <- readRDS("02_data_derived/treatment_compliance_all_rand_id.rds")
#' tmp <- rbindlist(lst, fill=TRUE)
#' tmp1 <- tmp[!is.na(rand_id) & variable == "madrs" & part == "total10"]
#' tmp2 <- merge(tmp1, dontrt, by.x = c("ID", "date"), by.y = c("ID", "d_on_treat"), 
#'               all.x = TRUE, sort = FALSE)
#' 
#' # Get missing status for MADRS10:
#' get_unique_visit_data(tmp2) |> get_missing_state()
#' 
get_missing_state <- function(data) {
  # Check arguments:
  stopifnot(is.data.table(data) && nrow(data) > 0)
  required_cols <- c("ID", "n_target", "state")
  test_required(required_cols, names(data), "data")
  .d <- copy(data)[,  .SD, .SDcols = required_cols] 
  setorder(.d, ID, n_target)
  
  last_target <- .d[, max(n_target, na.rm = TRUE)]
  .d[state == "in_window", missing := "in_window"]
  .d[state != "in_window", missing := "intermittent"]
  .d[state != "in_window", n_last_target := max(n_target), ID]
  
  ## Get most recent visit with missing data per ID:
  # Assign group numbers (take values 1 or 2) based on missing status
  for(id in .d[, unique(ID)]) .d[ID == id, grp := .GRP, .(n_last_target)]
  # .d[, grp := .GRP, .(n_last_target)]
  .d[, grp2 :=shift(grp, type = "lag"), ID]
  # Flag rows where transition from grp=1 (data recorded in window) to grp=2 
  # (missing data) occur
  .d[, k := 1 * ((grp == 2) & (grp2 == 1))]
  # Collect IDs that have at least one visit with missing data
  id_w_k1 <- .d[k == 1, unique(ID)]
  # Record latest visit with missing data per individual as latest_dropout. (NA 
  # for individuals with no missing data) 
  if(length(id_w_k1)) {
    .d[ID %in% id_w_k1, latest_dropout := max(n_target[k == 1]), by=ID]
  } else .d[, latest_dropout := NA_real_] 
  
  # Record latest visit with data in window
  id_w_in_window <- .d[state == "in_window", unique(ID)]
  .d[ID %in% id_w_in_window, last_in_window := max(n_target[state == "in_window"]), ID]

  # Classify missing state of obs. as dropout if all later observations are not
  # in window
  .d[!is.na(n_last_target) & n_target >= latest_dropout & 
       latest_dropout > last_in_window, missing := "dropout"]
  
  # Classify missing state of obs. as unknown if the visit window has not closed
  .d[state %in% c("now", "future"), missing := "unknown"]
  .d[, .(ID, n_target, state, missing, last_in_window)]
}


#' Add derived variables to data
#'
#' @param data a `data.table` to add variables to. Required columns are 
#' `c("ID", "date")`
#' @param which a character vector indicating which variables to add to `data`
#'
#' @returns the `data` `data.table` with `which` columns appended.
#'
#' @examples
#' 
#' lst <- readRDS("02_data_derived/data_by_category.rds")
#' d0 <- rbindlist(lst, fill = TRUE)
#' add_derived_data(d0)
#' add_derived_data(d0, "trt")
#' try(add_derived_data(d0, "typo")) # error
#' 
add_derived_data <- function(data, which = c("trt", "c_SAS", "d_day0", "i_on_treat")) {
  # Check arguments:
  stopifnot(is.data.table(data) && nrow(data) > 0,
            is.character(which) && length(which) >= 1L)
  # required_cols <- c("ID", "date")
  test_required("ID", names(data))
  all_which <- eval(expr = formals(add_derived_data)$which)
  for(wh in which) test_arg_in(all_which, wh, name = "which")

  # Select columns in 'data' not already in 'which':
  get_cols <- setdiff(names(data), which)
  .d <- copy(data)[, .SD, .SDcols = get_cols]

  # Read and add derived variables to data:
  if("trt" %in% which) {
    dtrt <- readRDS("02_data_derived/treatment_arm.rds")
    .d <- merge(.d, dtrt, by = "ID", all.x = TRUE, sort = FALSE)
  }
  if("c_SAS" %in% which) {
    dsas <- readRDS("02_data_derived/SAS_population.rds")
    .d <- merge(.d, dsas, by = "ID", all.x = TRUE, sort = FALSE)
  }
  if("d_day0" %in% which) {
    dt_day0 <- readRDS("02_data_derived/day0_all_subjects.rds")
    .d <- merge(.d, dt_day0[, .(ID, d_day0)], by = "ID", all.x = TRUE, sort = FALSE)
  }
  if("i_on_treat" %in% which) {
    test_required("date", names(data))
    dontrt <- readRDS("02_data_derived/treatment_compliance_all_rand_id.rds")
    .d <- merge(.d, dontrt[, .(ID, date = d_on_treat, i_on_treat)], 
                by = c("ID", "date"), all.x = TRUE, sort = FALSE)
  }
  
  .d[]
}


#' Get derived data
#' 
#' Read and return `"02_data_derived/data_by_category.rds"` with variables 
#' `c("trt", "c_SAS", "i_on_treat")` appended.
#' 
#' @param add_category a `category` column to the returned `data.table`? 
#' (length 1 logical; default is `TRUE`)
#'
#' @returns `data.frame` with derived data
#'
#' @examples
#' 
#' get_derived_data() 
#' 
get_derived_data <- function(add_category = TRUE) {
  stopifnot(is.logical(add_category) && length(add_category) == 1L)
  lst <- readRDS("02_data_derived/data_by_category.rds")
  if(add_category)
    lst <- lapply(names(lst), function(cat) cbind(category = cat, lst[[cat]]))
  d0 <- rbindlist(lst, fill = TRUE)
  add_derived_data(d0)
}


#' Get analysis data
#' 
#' Read and return `"02_data_derived/analysis_data.rds"`
#'
#' @returns
#'
#' @examples
#' 
#' get_analysis_data()
#' 
get_analysis_data <- function() {
  readRDS("02_data_derived/analysis_data.rds") 
}

