
#' Format p-values to set digits
#'
#' @param p vector of p-values (numerical vector)
#' @param digits number of desired digits. The default (3) returns `"<0.001"` 
#' for`p < 0.001`.  
#' @param warn_on_bad_p issue warning if any `p` outside `[0, 1]`? (save for a 
#' tolerance)
#'
#' @returns character vector of formatted p-values.
#'
#' @examples
#' 
#' dig <- 3
#' x <- c(Inf, 2, 1, 1 - 1e-10, 0.1, .012345, 0.001, 0.0010001, 0.001000000001, .0005, 
#'        .0009, .0009999999, 1e-27, 0,  -1e-10, -1e-6, -1, -Inf, NA, NaN)
#' cbind(x, 
#'       format.pval  = format.pval(x, eps = 10^(-dig)),
#'       format_pval2 = format_pval2(x, digits=dig))
#'       
format_pval2 <- function(p, digits = 3, warn_on_bad_p = TRUE) {
  stopifnot(is.numeric(p) && length(p) >= 1L,
            is.numeric(digits) && length(digits) == 1L && is.finite(digits),
            abs(digits - round(digits)) <= sqrt(.Machine$double.eps),
            digits >= 1 && digits <= 20,
            is.logical(warn_on_bad_p) && length(warn_on_bad_p) == 1L &&
              !is.na(warn_on_bad_p))
  stopifnot(requireNamespace("CRP", quietly = TRUE),
            requireNamespace("data.table", quietly = TRUE))
  
  digits <- round(digits)
  eps <- 10^(-digits)
  
  # Warn on p-values outside [0, 1]:
  tol <- sqrt(.Machine$double.eps)
  is_bad_p <- !is.na(p) & (p < -tol | p > 1 + tol)
  if(warn_on_bad_p && any(is_bad_p)) 
    warning(sum(is_bad_p), " p-value(s) outside [0, 1]: ", 
            paste(p[is_bad_p], collapse = ", "))
  
  data.table::fcase(
    is.na(p), NA_character_, 
    p >= 1,   CRP::do_format(1, digits = digits),
    p < eps,  paste0("<", CRP::do_format(eps, digits)),
    default = CRP::do_format(p, digits = digits))
}

#' Format confidence interval for display
#'
#' @details If argument `lwr` is a length two numeric vector its values are
#'   assumed to be the lower and upper values of the confidence interval and
#'   argument `upr` is ignored.
#'
#'
#' @param lwr lower confidence limit (scalar numeric)
#' @param upr upper confidence limit (scalar numeric)
#' @param digits number of decimal places determined by `CRP::do_format()`
#'   (scalar integer)
#' @param sep separator, eg. `"; "` or similar (scalar character)
#'
#' @returns The confidence interval formatted in the style "(lwr.xx - upr.xx)".
#'
# #' @examples
format_ci <- function(lwr, upr, digits=2, sep = " - ") {
  if(length(lwr) == 2){
    upr <- lwr[2]
    lwr <- lwr[1]
  }
  paste0("(", do_format(lwr, digits), sep, do_format(upr, digits), ")")
}

cap_first_letter <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2)) 
}
# x <- "the quick red fox jumps over the lazy brown dog"
# cap_first_letter(x)

## General convenience function
##
## Arguments: 
## - The same arguments as 'grep'
## 
## Return value:
## - The result of `x[grep(pattern, x)]`
## 
getgrep <- function(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, 
                    fixed = FALSE, useBytes = FALSE, invert = FALSE) {
  x[grep(pattern, x, ignore.case, perl, value, 
         fixed, useBytes, invert)]
}





