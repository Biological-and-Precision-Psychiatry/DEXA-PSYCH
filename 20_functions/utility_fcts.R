
#' Un-directional set differences
#' 
#' `base::setdiff()` is directional meaning that `base::setdiff(x, y)` is
#' generally different from `base::setdiff(y, x)`. `setDiff` is un-directional
#' so `setDiff(x, y)` is the same as `setDiff(y, x)`.
#' 
#' @param x,y vectors of items
#' 
#' @returns Vector of un-directional set differences
#'
#' @examples

#' # Example data:
#' x <- c("a", "b", "c")
#' y <- c("b", "c", "d")
#' 
#' # Directional set diffs:
#' setdiff(x, y)
#' setdiff(y, x)
#' 
#' # Un-directional set diffs:
#' setDiff(x, y)
#' setDiff(y, x)
#' 
setDiff <- function(x, y, ...) unique(c(setdiff(x, y, ...), setdiff(y, x, ...)))


#' Named list
#'
#' @param ... elements (possibly named) to be joined in a `list`
#'
#' @returns a named list, where the names are the `deparse()`'d actual argument 
#' names
#'
#' @examples
#' 
#' a <- 1
#' b <- 2
#' c <- 3
#' d <- list(e=2, f=factor(letters[rep(1:2, 2)]))
#' g <- matrix(runif(9), 3)
#' 
#' namedList(a, b, c)
#' namedList(a, b, c, d, g)
#' 
#' res <- namedList(d, g)
#' names(res)
#' 
namedList <- function(...) {
  setNames(list(...), nm = sapply(as.list(match.call()), deparse)[-1])
}


#' Extract the elements `x` that occur in `y`
#'
#' @param x,y vector of objects
#'
#' @returns the elements `x` that occur in `y`
#'
#' @examples
#' 
#' a <- letters[1:10]
#' b <- letters[8:12]
#' get_in(a, b)
#' 
get_in <- function(x, y) { x[x %in% y] } 



#' Test that required columns occur in actual data.
#'
#' @param required vector of required columns (character vector)
#' @param actual vector of actual columns (character vector)
#' @param name name of 'data' argument (scalar character)
#'
#' @returns nothing, but an error is thrown if any 'required' do not occur in
#' 'actual'
#'
#' @examples
#' 
#' # Example data:
#' d <- data.table(A = letters[1:10],
#'                 x = 1:10)
#' 
#' # These are OK:
#' test_required(c("A", "x"), names(d))
#' test_required(c("A"), names(d))
#' 
#' # This gives an error:
#' try(test_required(c("A", "Y"), names(d)))
#' 
test_required <- function(required, actual, name = "data") {
  missing_cols <- setdiff(required, actual)
  if (length(missing_cols) > 0)
    stop(sprintf("'%s' is missing required column(s): %s",
                 name, paste(missing_cols, collapse = ", ")))
}


#' Test that actual arguments are a subset of valid arguments
#'
#' @param valid vector of all valid arguments
#' @param actual vector of actual arguments
#' @param name name of argument
#'
#' @returns nothing, but an error is thrown if any 'actual' do not occur in
#' 'valid'
#'
#' @examples
#' 
#' # Example data:
#' d <- data.table(A = letters[1:10],
#'                 x = 1:10)
#' 
#' # This is OK:
#' test_arg_in(c("A", "x"), names(d))
#' 
#' # These give errors:
#' try(test_arg_in(c("A"), names(d)))
#' try(test_arg_in(valid = c("A", "Y"), names(d)))
#' 
test_arg_in <- function(valid, actual, name = "which") {
  offending <- setdiff(actual, valid)
  if(length(offending) > 0)
    stop(sprintf("'%s' is not among valid options for '%s': %s",
                 paste(actual, collapse = ", "), name, 
                 paste(valid, collapse = ", ")))
}


#' Check if a variable is a non-NA, positive length character
#'
#' @param x vector to check
#'
#' @returns logical vector of same length as `x`
#'
#' @examples
#' 
#' has_char("Statistician")  # TRUE
#' has_char("a")             # TRUE
#' has_char("")              # FALSE
#' has_char(3)               # FALSE
#' has_char(NA_character_)   # FALSE
#' has_char(NA_real_)        # FALSE
#' has_char(Inf)             # FALSE
#' has_char(NULL)            # logical(0)
#' has_char(character(0L))   # logical(0)
#' has_char(character(1L))   # FALSE
#' has_char(character(2L))   # FALSE FALSE
#' 
has_char <- function(x) {
  vapply(x, function(xx) { 
    !is.na(xx) && is.character(xx) && nchar(xx) > 0L
  }, FUN.VALUE = logical(1L), USE.NAMES = FALSE)
}

#' Get date of data extraction from REDCap database
#' 
#' Extract the date from the saved checksums using 
#' `get_datetime_from_checksums()`.
#'
#' @returns The REDCap data extraction date.
#'
#' @examples
#' 
#' get_db_date()
#' 
get_db_date <- function() {
  datetime <- get_datetime_from_checksums()
  dbdate <- substr(datetime, 1, 10)
  dbdate <- as.Date(dbdate)
  if(!inherits(dbdate, "Date")) stop("Unable to extra database date")
  dbdate
}


#' Remove all-NA columns from `data.table`
#'
#' @param data `data.table` with potential all-NA columns
#'
#' @returns `data.table` with all-NA columns removed
#'
#' @examples
#' # Example data:
#' d0 <- get_derived_data()
#' 
#' # Remove all-NA columns:
#' d1 <- d0[category == "blod_data"] |> rm_na_cols()
#' 
#' # Names of removed columns:
#' setDiff(names(d1), names(d0))
#' 
rm_na_cols <- function(data) {
  stopifnot(is.data.table(data) && nrow(data) > 0 && ncol(data) > 0)
  all_na <- sapply(data, function(v) all(is.na(v)))
  keep <- setdiff(names(data), names(which(all_na)))
  data[, .SD, .SDcols = keep][]
}


#' Sort by number; then by character
#'
#' @param x a vector of characters potentially including numerals
#'
#' @returns sorted vector
#'
#' @examples
#' 
#' x <- c("ab", "zzzz", "12", "an9", "hi7", "x3y", "an3", "2")
#' sort_by_number(x)
#' x <- 10:4
#' sort_by_number(x)
#' 
#' x <- list("a", "x", matrix(4:7, 2), 1)
#' try(sort_by_number(x))
#' try(sort_by_number(c("NA", NA)))
#' 
sort_by_number <- function(x) {
  stopifnot(
    "'x' must be an atomic vector" = is.atomic(x),
    "'x' must be a vector, not a matrix or array" = is.null(dim(x)),
    "'x' must not be empty" = length(x) > 0,
    "'x' must not contain NA" = sum(is.na(x)) == 0
  )
  
  if(is.factor(x))  x <- as.character(x)
  if(is.numeric(x)) x <- as.character(x)
  
  # Sort by number; then by text:
  nums <- as.numeric(gsub("[^0-9]", "", x))
  nums[is.na(nums)] <- Inf
  
  text_part <- gsub("[0-9]", "", x)
  
  x[order(nums, text_part)]
}

#' Make f_visit from c_visit
#'
#' @param data a `data.table` with columns `c_visit` or `f_visit`
#' @param rm_c_visit remove the `c_visit` column?
#'
#' @returns `data` with a column `f_visit` with the correct levels corresponding
#' to `get_visits()`
#'
#' @examples
#' 
#' get_visits()
#' dt <- data.table(x = 1:100, 
#'                  c_visit = sample(c("day0", "day7", "day28"), 100,
#'                                   replace = TRUE))
#' res <- set_f_visit(dt)
#' head(res)
#' all(dt[, c_visit] == res[, as.character(f_visit)])
#' res[, levels(f_visit)]
#' 
set_f_visit <- function(data, rm_c_visit = TRUE) {
  stopifnot(is.data.table(data) && nrow(data) > 0,
            any(c("c_visit", "f_visit") %in% names(data)),
            is.logical(rm_c_visit) && length(rm_c_visit) == 1)
  has_c_visit <- ("c_visit" %in% names(data))
  has_f_visit <- ("f_visit" %in% names(data))
  .d <- copy(data)
  
  if(!has_c_visit) .d[, c_visit := as.character(f_visit)]
  stopifnot(.d[, all(c_visit %in% get_visits())])
  
  .d[, f_visit := factor(c_visit, levels = get_visits())]
  stopifnot(all(.d[, levels(f_visit)] == get_visits()),
            .d[, all(c_visit == as.character(f_visit))])
  if(rm_c_visit) .d[, c_visit := NULL]
  
  .d[]
}

get_visits <- function() c("day0", "day4", "day7", "day14", "day28", "month6")

#' Get treatment levels in data
#'
#' @param data 
#' @param ref Search string for reference level
#' @param active Search string for reference level 
#'
#' @returns a length-2 character vector with elements ("reference", "active") 
#' in that order.
#'
#' @examples
#' 
#' # Get treatment levels:
#' get_analysis_data() |> get_trt_levs()
#' 
#' # Expect error here:
#' get_analysis_data() |> get_trt_levs(ref = "tmp")
#' 
get_trt_levs <- function(data, ref = "[Pp]lacebo", active = "[Dd]exa") {
  required_cols <- "trt"
  test_required(required_cols, names(data), name = "data")
  stopifnot()

  trt_levs <- data[, if(is.factor(trt)) levels(trt) else unique(trt)]
  Ref <- getgrep(ref, trt_levs)
  Act <- getgrep(active, trt_levs)
  if(!length(Ref) || is.na(Ref)) stop("Reference level not found in 'data'")
  if(!length(Act) || is.na(Act)) stop("Active level not found in 'data'")
  c("reference" = if(length(Ref)) Ref else NA_character_, 
    "active" = if(length(Act)) Act else NA_character_)
}


#' Get `data.table` results for a metadata row
#' 
#' The relevant `.rds`-file in `50_output_db/outputs/` is located from the 
#' metadata argument and the results table corresponding to the relevant endpoint
#' id is extracted and returned.
#' 
#' Requires that the pipeline has been run to produce the relevant results.
#'
#' @param regi a one-row metadata `data.table` originating from `get_metadata()`.
#' `regi` has to specify a `table` produced by the pipeline.
#' @return return a single table (`"table"`) or the entire list of tables 
#' available in the file (`"list"`)
#'
#' @returns a `data.table` with results corresponding to `regi` metadata.
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
#' # get_dt_table(regi_all[1]) # requires pipeline has been run
#' 
#' # Get all tables in the file:
#' # get_dt_table(regi_all[1], "list") |> length() # requires pipeline has been run
#' 
get_dt_table <- function(regi, return = c("table", "list")) {
  return <- match.arg(return)
  eid <- regi[, endpoint_id]
  FILE <- paste0("50_output_db/outputs/", regi[, outcome_var], 
                 "_", regi[, estimand], ".rds")
  if(!file.exists(FILE)) stop("File does not exist: ", FILE)
  dttabs <- readRDS(file = FILE)
  if(return == "list") return(dttabs)
  if(!eid %in% names(dttabs)) stop("No table in file for endpoint id: ", eid)
  dttabs[[eid]][]
}


#' Get function definition from file
#'
#' @param file character: name of file
#' @param fun_name character: name of function 
#' @param roxygen include roxygen comments (`#` or `#'`) above the file definition?
#'
#' @returns character vector corresponding to the lines of the function 
#' definition including all comments.
#'
#' @examples
#' 
#' get_function_lines("20_functions/utility_fcts.R", "get_function_lines")
#' 
#' get_function_lines("20_functions/utility_fcts.R", "get_function_lines", 
#'                    roxygen = TRUE)
#' 
get_function_lines <- function(file, fun_name, roxygen = FALSE) {
  stopifnot(is.character(file) && length(file) == 1L,
            file.exists(file),
            is.character(fun_name) && length(fun_name) == 1L && 
              !is.na(fun_name) && nchar(fun_name) > 0,
            is.logical(roxygen) && length(roxygen) == 1L && !is.na(roxygen))
  
  # Assess file:
  exprs <- parse(file, keep.source = TRUE)
  srcrefs <- attr(exprs, "srcref")
  
  # Walk through all potential function definitions in the file:
  for (i in seq_along(exprs)) {
    
    # Identify 'fun_name' in file:
    e <- exprs[[i]]
    e_is_fun_name <- is.call(e) && as.character(e[[1]]) %in% c("<-", "=") &&
      identical(as.character(e[[2]]), fun_name)
    if (e_is_fun_name) {
      # Grab lines in file for 'fun_name':
      ref <- srcrefs[[i]]
      fn_lines <- as.character(ref)
      
      if (!roxygen) return(fn_lines)
      
      # Include roxygen comments:
      # Walk upward from the function's start line, collecting a
      # contiguous block of comment lines ("#" or "#'") with no
      # blank-line gap allowed between the block and the function
      all_lines <- readLines(file)
      start_line <- ref[1]
      
      j <- start_line - 1
      while (j >= 1 && grepl("^\\s*#", all_lines[j])) j <- j - 1
      roxy_start <- j + 1
      roxy_end <- start_line - 1
      if(roxy_start > roxy_end) return(fn_lines) # no roxygen comments

      return(c(all_lines[roxy_start:roxy_end], fn_lines))
    }
  }
  stop("Function '", fun_name, "' not found in ", file)
}


#' Grow a list of tables
#' 
#' Helper-function to assist in collecting tables for saving list of tables in 
#' an `.rds` file. Tables are named by its endpoint id.
#'
#' @param table a `data.table` to save to a list
#' @param regi a metadata row
#' @param original optional list to grow
#'
#' @returns a named list of tables.
add2list <- function(table, regi, original = NULL) {
  stopifnot(is.data.table(table),
            is.data.table(regi) && nrow(regi) == 1L,
            is.null(original) || is.list(original))
  new <- list(table) |> setNames(nm = regi[1L, endpoint_id])
  if(is.null(original)) new else c(original, new)
}


