

#' Get footnotes source data
#' 
#' The footnotes source data is a `data.table` with three columns:
#' 1. `type`  : scale, abbreviation, definition, qualifier, or analysis
#' 2. `key`   : term in table/figure that needs explanation via footnote
#' 3. `footer`: the footnote
#'
#' @returns
#'
#' @examples
#' # Get all footnotes source data:
#' footers <- get_footers()
#' 
#' # Footer for CRP scale:
#' footers[key == "CRP"]
#' 
#' # Footer for BMI abbreviation:
#' footers[key == "BMI"]
#' 
get_footers <- function() {
  fread(file = "10_metadata/table_footers.csv")
}


#' Make list of footers for a table or figure
#'
#' @param regi a one-row metadata `data.table` originating from `get_metadata()`
#' @param dttab an optional `data.table` object with analysis results
#' @param table_footers an optional `data.table` with footer source data 
#' originating from a call to `get_footers()`
#'
#' @returns a named list of footnotes. Possible elements are `scale`, `abbrev`,
#' `f_qual`, `model`, and `definition`.
#'
#' @examples
#' 
#' # Get metadata:
#' tlf_reg  <- get_metadata()
#' regi_all <- tlf_reg[chapter == "madrs_total10"]
#' regi <- regi_all[3]
#' 
#' # Read statistical results table:
#' dttab <- get_dt_table(regi)
#' 
#' # Make list of footnotes:
#' mk_footer_list(regi, dttab)
#' 
#' # Footnotes for a figure:
#' regi <- regi_all[analysis == "chgbl-time"][1L]
#' mk_footer_list(regi = regi)
#' 
#' # Footnotes also specified in metadata:
#' regi <- tlf_reg[nchar(footer) > 0][1]
#' mk_footer_list(regi)
#' 
#' regi <- tlf_reg[endpoint_id == "P02-32"]
#' regi <- tlf_reg[endpoint_id == "S32-01"]
#' regi <- tlf_reg[endpoint_id == "S32-03"]
#' dttab <- get_dt_table(regi)
#' mk_footer_list(regi, dttab = dttab)
#' 
mk_footer_list <- function(regi, 
                           dttab         = data.table(NULL), 
                           table_footers = NULL) {
  # Check arguments:
  stopifnot(is.data.table(dttab),
            is.data.table(regi) && nrow(regi) == 1L)
  if(is.null(table_footers)) table_footers <- get_footers()

  # Parameters:
  analysis  <- regi[, analysis]
  role      <- regi[, role]
  outc_kind <- regi[, outcome_kind]
  subjects  <- regi[, subjects]
  estimand  <- regi[, estimand]

  # Scale and abbreviation footers:
  res <- list(scale  = mk_footer_scale(regi, table_footers),
              abbrev = mk_footer_abbrev(regi, dttab, table_footers))
  
  # Get footer for qualifiers:
  qual_footers <- table_footers[type == "qualifier"]
  f_estimand <- if(regi[, role] == "Summary") NULL else
    qual_footers[key == estimand, footer]
  if(regi[, grepl("cens", transformation)] && regi[, analysis == "histogram"])
    f_estimand <- "Presentation includes uncensored data."
  f_subjects <- qual_footers[key == subjects, footer]
  f_trt <- switch (estimand,
                   "policy"   = qual_footers[key == "any_treatment", footer],
                   "efficacy" = qual_footers[key == "on_treatment", footer])
  res$f_qual <- trimws(paste(f_estimand, f_subjects, f_trt))

  # Get footer for analysis
  model_footers <- table_footers[type == "analysis"]
  # regi[, .(role, analysis, analysis_label, imputation, imputation_label)]
  if(regi[, role] != "Summary") { # get footer for analysis
    f_model <- "Unknown analysis footer."
    if(grepl("ancova", analysis))
      f_model <- model_footers[key == "ANCOVA", footer]
    if(grepl("cens-ancova", analysis))
      f_model <- model_footers[key == "cens-ANCOVA", footer]
    if(regi[, imputation] %in% "sipw")
      f_model <- model_footers[key == "sipw-ANCOVA", footer]
    if(grepl("cum-sipw", regi[, imputation]))
      f_model <- model_footers[key == "cum-sipw-ANCOVA", footer]
    if(grepl("mmrm", analysis))
      f_model <- model_footers[key == "MMRM", footer]
    if(grepl("mmrm", analysis) & grepl("[Ss]ubgroup", role))
      f_model <- model_footers[key == "subgroup-MMRM", footer]
    if(grepl("chgbl-time", analysis)) 
      f_model <- NULL
    if(grepl("t-test", analysis))
      f_model <- model_footers[key == "ttest", footer]
    if(grepl("logistic", analysis)) 
      f_model <- model_footers[key == "logistic", footer]
    if(grepl("responder|remission|sustain", analysis) & 
       grepl("sipw", regi[, imputation]))
      f_model <- model_footers[key == "logistic_reg_sipw", footer]
    if(grepl("responder|remission|sustain", analysis) & 
       grepl("cca", regi[, imputation]))
      f_model <- model_footers[key == "logistic_reg_cca", footer]
    if(grepl("responder|remission|sustain", analysis) & 
       regi[, imputation] == "mmrm")
      f_model <- model_footers[key == "logistic_reg_mmrm", footer]
    if(grepl("responder|remission|sustain", analysis) & 
       regi[, imputation] == "mmrm-uce")
      f_model <- model_footers[key == "uncond_exact_mmrm", footer]
    if(grepl("responder|remission|sustain", analysis) & 
       regi[, imputation] == "cca-uce")
      f_model <- model_footers[key == "uncond_exact_cca", footer]
    if(grepl("tipping1", analysis))
      f_model <- model_footers[key == "tipping1", footer]
    if(grepl("tipping2", analysis))
      f_model <- model_footers[key == "tipping2", footer]
    res$model <- f_model
  }
  
  # Get footer for definitions
  definition_footers <- table_footers[type == "definition"]
  if(grepl("responder", outc_kind)) 
    res$definition <- definition_footers[key == "responder", footer]
  if(grepl("remission", outc_kind)) 
    res$definition <- definition_footers[key == "remission", footer]
  if(grepl("sustain", outc_kind)) 
    res$definition <- definition_footers[key == "sust_resp", footer]

  # Merge with footers defined in metadata:
  regi_footers <- get_footers_from_regi(regi)
  regi_footers <- regi_footers[!names(regi_footers) %in% "abbrev"]
  if(length(regi_footers) > 0) res <- merge_lists(res, regi_footers)
  
  # Return final footer
  res
}


#' Make abbreviation footnotes
#'
#' @inheritParams mk_table_footers
#'
#' @returns a character vector of abbreviation footers.
#'
#' @examples
#' 
#' # Get metadata:
#' tlf_reg  <- get_metadata()
#' regi_all <- tlf_reg[chapter == "madrs_total10"]
#' regi <- regi_all[3]
#' 
#' # Read statistical results table:
#' dttab <- get_dt_table(regi)
#' 
#' # Make list of footnotes:
#' mk_footer_abbrev(regi, dttab)
#' 
#' # Footnotes for a figure:
#' regi <- regi_all[analysis == "chgbl-time"][1L]
#' mk_footer_abbrev(regi = regi)
#' 
mk_footer_abbrev <- function(regi, 
                             dttab         = data.table(NULL), 
                             table_footers = NULL) {
  # Check arguments:
  stopifnot(is.data.table(dttab),
            is.data.table(regi) && nrow(regi) == 1L)
  if(is.null(table_footers)) table_footers <- get_footers()

  match_vec_to_keys <- function(vec, keys) {
    .k <- keys
    .k[.k == "N"] <- "^N$"
    .k[.k == "CI"] <- "CI\\)"
    # .k[.k == "SD"] <- "\\(SD"
    # .k[.k == "SD"] <- "(?<!g)SD" # look behind and assert that g is not there
    .k[.k == "SD"] <- "[^g]SD|^SD" # matches 'not-g+SD' or 'SD' at start of string
    res <- lapply(vec, function(nm) { # nm <- "FAS"
      x <- sapply(.k, function(key) grepl(key, nm))
      if(any(x)) keys[x] else ""
    }) |> unlist()
    res[nchar(res) > 0L]
  }
  
  # Look for abbreviation keys in outcome_label, caption and names(dttab)
  abbrev_footers <- table_footers[type == "abbreviation"]
  if(regi[, transformation] == "log") 
    abbrev_footers[key == "Miss", 
                   footer := gsub("missing", "missing or non-positive", footer)]
  if(grepl("log", regi[, transformation])) {
    abbrev_footers[key == "Effect", footer := gsub("difference", "ratio", footer)]
    abbrev_footers[key == "Effect", footer := gsub("-", "/", footer, fixed = TRUE)]
  }
  keys <- abbrev_footers[, key]
  nms <- names(dttab)
  
  # Keys for outcome_label (ol), table title (tt), table headers (th), and
  #  column content (cc):
  keys_ol <- get_in(strsplit(regi[, outcome_label], split="\\s")[[1L]], keys)
  keys_tt <- get_in(strsplit(regi[, caption], split="\\s")[[1L]], keys)
  keys_th <- match_vec_to_keys(nms, keys)
  keys_cc <- lapply(nms, function(nm) {
    match_vec_to_keys(dttab[, get(nm)], keys)
  }) |> unlist()
  
  # keep first match to get right order of abbreviations:  
  use_keys <- c(keys_ol, keys_tt, keys_th, keys_cc)
  use_keys <- use_keys[!duplicated(use_keys)]

    
  footer <- sapply(use_keys, function(uk) abbrev_footers[key == uk, footer]) 
  
  # Merge with footers defined in meta-data:
  regi_footers <- get_footers_from_regi(regi)[["abbrev"]]
  if(length(regi_footers) > 0) footer <- c(footer, regi_footers)
  
  # Return
  footer <- paste(footer, collapse = ", ") 
  if(nchar(footer) > 0) paste0(footer, ".") else NULL
}

#' Get footers from direct metadata specification
#'
#' @param regi 
#' @param table_footers 
#'
#' @returns a named list of footer contributions
#' @examples
#' 
#' # Get metadata:
#' tlf_reg  <- get_metadata()
#' 
#' # Get footers from metadata-row:
#' regi <- tlf_reg[analysis == "chgbl-time"][1]
#' get_footers_from_regi(regi)
#' 
#' # Get footers from metadata-row:
#' regi <- tlf_reg[role == "Subgroup" & tlf_type == "figure"][1]
#' get_footers_from_regi(regi)
#' 
get_footers_from_regi <- function(regi, table_footers = NULL) {
  # Check arguments:
  stopifnot(is.data.table(regi) && nrow(regi) == 1L)
  if(is.null(table_footers)) table_footers <- get_footers()
  
  regi_footers <- regi[, footer]
  if(is.na(regi_footers) || nchar(regi_footers) <= 0) return(list())
  
  tmp <- parse(text = regi_footers) |> eval()
  
  # Swap "footer_" for abbreviation in footer table:
  tmp[] <- lapply(seq_along(tmp), function(i) { # i <- 1
    obj <- tmp[[i]]
    keys_id <- grep("footer_", obj)
    keys <- gsub("footer_", "", obj[keys_id])
    obj[keys_id] <- sapply(keys, function(Key) {
      res <- table_footers[key == Key, .SD[1]][, footer]
      if(length(res)) res else ""
    })
    obj
  })
  tmp
}


#' Make scale footers
#'
#' @inheritParams mk_footer_list
#'
#' @returns a character vector with the scale footer.
#'
#' @examples
#' 
#' # Get metadata:
#' tlf_reg  <- get_metadata()
#' regi_all <- tlf_reg[chapter == "madrs_total10"]
#' regi <- regi_all[3]
#' 
#' # Read statistical results table:
#' dttab <- get_dt_table(regi)
#' 
#' # Make list of footnotes:
#' mk_footer_scale(regi)
#' 
mk_footer_scale <- function(regi, 
                            table_footers = NULL) {
  # Check arguments:
  stopifnot(is.data.table(regi) && nrow(regi) == 1L)
  if(is.null(table_footers)) table_footers <- get_footers()

  scale_footers <- table_footers[type == "scale"]
  keys <- scale_footers[, key]
  use <- sapply(keys, function(key) any(grepl(key, regi[, outcome_label]))) |
    sapply(keys, function(key) any(grepl(key, regi[, caption]))) 
  use_keys <- keys[use][1L]
  
  # Return
  footer <- scale_footers[key %in% use_keys, footer]
  if(length(footer) > 0 && any(nchar(footer) > 0)) paste0(footer[1L], ".") else NULL
} 

#' Merge named lists, concatenating elements with matching names
#'
#' Combines two or more named lists into a single list. When the same
#' name appears in multiple input lists, the corresponding elements are
#' concatenated (via `unlist()`) in the order the lists were supplied.
#' Names are kept in first-seen order: names from the first list appear
#' first (in their original order), followed by any new names introduced
#' by subsequent lists (also in their original order).
#'
#' @param ... Named lists to merge. Each list should have (at least
#'   partially) named elements; elements are combined by name across
#'   all lists supplied.
#'
#' @return A named list containing the union of names from all input
#'   lists, with values from matching names concatenated in the order
#'   the lists were passed to \code{...}.
#'
#' @examples
#' L1 <- list(A = c("a", "b"), F = "g")
#' L2 <- list(C = "d", F = c("h", "i"))
#' merge_lists(L1, L2)
#' #> $A
#' #> [1] "a" "b"
#' #>
#' #> $F
#' #> [1] "g" "h" "i"
#' #>
#' #> $C
#' #> [1] "d"
#'
merge_lists <- function(...) {
  lists <- list(...)
  keys  <- unique(unlist(lapply(lists, names)))
  setNames(lapply(keys, function(k) unlist(lapply(lists, `[[`, k))), keys)
}
