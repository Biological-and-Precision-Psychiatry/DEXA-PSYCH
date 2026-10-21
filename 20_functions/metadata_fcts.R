imputation_map_txt <- "
imputation, imputation_label
cca,        Complete Case Analysis
cca-uce,    Complete Case Analysis
cum-sipw,   Cumulative stabilized inverse probability of censoring weighting
sipw,       Stabilized inverse probability of censoring weighting
mmrm,       MMRM Imputation of missing values
mmrm-uce,   MMRM Imputation of missing values"

analysis_map_txt <- "
analysis,      analysis_label
summary,       Summary of observed data
histogram,     Histogram of observed data
t-test,        Two-sample *t*-test
ancova,        ANCOVA
mmrm,          MMRM
tipping1,      One-way tipping point analysis
tipping2,      Two-way tipping point analysis
cens-ancova,   Censored log-normal model
cens-t-test,   Censored log-normal model
logistic,      Logistic regression
responder,     Responder analysis
remission,     Remission analysis
sustain,       Sustained response analysis
chgbl-time,    Change over time"

outcome_kind_map_txt <- "
outcome_kind, oc_kind_label
value,        
binary,
score,        Score
chgbl,        Change-from-baseline
responder,    Responder
remission,    Remission
sustain,      Sustained response" 

outcome_type_map_txt <- "
outcome_kind, outcome_type
value,        cont
score,        cont
chgbl,        cont
binary,       binary
responder,    binary
remission,    binary
sustain,      binary" 

valid_visits_map_txt <- "
data_name; valid_visits
madrs;     0,4,7,14,28,182
hamd;      0,7,28,182
hama;      0,7,28,182
facitf;    0,7,28,182
mdi;       0,7,28,182
cssrs;     0,7,28,182
eq5d5l;    0,7,28,182
gaf;       0,7,28,182
saps;      0,7
ymrs;      0,7
gti;       7"

vitals <- 
  c("sysblodtryk", "diablodtryk", "puls", "temp", "respirationsfrekvens", 
    "hoejde", "vaegt", "qtc_interval")
biochemistry <- 
  c("erythrocytter", "hbg", "crp", "leukocytter", "basofil", "eosinofil", 
    "lymfocytter", "monocytter", "neutrofile", "metamyelopro", "trombocytter", 
    "blodsukker", "kalium", "kreatinin", "natrium", "alat", "fosfatase", 
    "hba1c", "tsh", "kolesterol", "hdl", "ldl", "triglycerider")

imputation_map <- fread(input = imputation_map_txt)
analysis_map <- fread(input = analysis_map_txt)
outcome_kind_map <- fread(input = outcome_kind_map_txt)
outcome_type_map <- fread(input = outcome_type_map_txt)
valid_visits_map <- rbind(
  fread(input = valid_visits_map_txt, sep = ";"),
  data.table(data_name    = c(vitals, biochemistry), 
             valid_visits = "0,7"))

get_valid_visits <- function(data_name) {
  # data_name <- "madrs"
  stopifnot(data_name %in% valid_visits_map[, data_name])
  .data_name <- data_name
  visit_txt <- valid_visits_map[.data_name == data_name, valid_visits]
  visits <- eval(expr = parse(text = paste0("c(", visit_txt, ")")))
  visits
}

#' Read and format metadata
#'
#' @returns a `data.table` with all metadata information
#'
#' @examples
#' 
#' tlf_reg <- get_metadata()
#' 
get_metadata <- function() {
  # Read metadata registry files:
  tlf_reg00 <- fread(file = "10_metadata/tlf_registry.csv")[is.na(exclude) | exclude < 1]
  tlf_reg00 <- tlf_reg00[, -c("exclude")]
  
  # Expand estimand:
  valid_estimands <- c("intro", "overview", "policy", "efficacy", "safety")
  tlf_reg00 <- lapply(valid_estimands, function(estim) {
    res <- tlf_reg00[grepl(estim, estimand)]
    res[, estimand := estim]
    res[]
  }) |> rbindlist()
  # For later ordering by estimand and row id:
  tlf_reg00[, estimand := factor(estimand, levels = valid_estimands)]
  tlf_reg00[, row_id := seq_len(.N)] 

  # Number within estimand:
  for(E in tlf_reg00[, unique(estimand)])
    tlf_reg00[estimand == E, outcome_num0 := .GRP , by = 
                .(data_variable, data_part, outcome_kind, outcome_var)]
  
  # Make outcome_num:
  tlf_reg00[, outcome_num := .GRP , by = .(estimand, outcome_num0)]
  
  # Make endpoint_num variable:
  tlf_reg00[, endpoint_num := seq_len(.N), by = outcome_num]
  
  # Add endpoint_id to tlf registry:
  tlf_reg00[, endpoint_id := paste0(toupper(substr(as.character(estimand), 1, 1)), 
                                    formatC(outcome_num0, flag="0", width=2), "-", 
                                    formatC(endpoint_num, flag="0", width=2))]
  tlf_reg00[, outcome_num0 := NULL]

  # Split manually and automatically generated outputs
  tlf_reg0 <- tlf_reg00[is.na(outcome_var) | nchar(outcome_var) == 0]    # auto
  tlf_reg1 <- tlf_reg00[!(is.na(outcome_var) | nchar(outcome_var) == 0)] # manual

  # Make 'data_name':  
  tlf_reg0[, data_name := fifelse(outcome_kind == "chgbl", "chgbl", "value")]
  
  # Make 'outcome_label' and 'outcome_type':
  tlf_reg0 <- merge(tlf_reg0, outcome_kind_map, by="outcome_kind",
                   all.x = TRUE, sort = FALSE)
  tlf_reg0 <- merge(tlf_reg0, outcome_type_map, by="outcome_kind",
                   all.x = TRUE, sort = FALSE)
  tlf_reg0[, outcome_label := trimws(paste(variable_label, oc_kind_label))]

  # Add analysis_label and imputation_label to tlf registry:
  tlf_reg0 <- merge(tlf_reg0, analysis_map, by = "analysis", 
                   all.x = TRUE, sort = FALSE)
  tlf_reg0 <- merge(tlf_reg0, imputation_map, by = "imputation", 
                   all.x = TRUE, sort = FALSE)
  
  # Add outcome_var to tlf registry:
  tlf_reg0[, outcome_var := paste(data_variable, data_part, sep="_")]
  
  # Add folder and file names to registry:
  if(tlf_reg0[, !all(has_char(tlf_type))]) 
    stop("'tlf_type' must be specified for auto-outputs")
  tlf_reg0[tlf_type == "table",  folder := "40_tlf_source/auto_tables"]
  tlf_reg0[tlf_type == "figure", folder := "40_tlf_source/auto_figures"]
  tlf_reg0[, file_stem := mk_file_stem(.SD), .I]

  # Generate 'chapter':
  tlf_reg0[!has_char(chapter), chapter := outcome_var]
  
  # Generate section and subsection titles:
  tlf_reg0[endpoint_num == 1, section := outcome_label]
  tlf_reg0[, num_in_role := 1:.N, by=.(estimand, outcome_num, role)]
  tlf_reg0[num_in_role == 1, 
          subsection := fifelse(role == "Summary", "Summary of observed data", 
                                paste(role, "analyses"))] 
  tlf_reg0[, num_in_role := NULL]
  # remove duplicate sections in sequence (eg. CRP):
  tlf_reg0[!is.na(section) & nzchar(section), 
          section := fifelse(section == shift(section), "", section, 
                             na = section)] # Note: shift(x)[1] is NA!

  # Generate captions for all auto-outputs:
  stopifnot(tlf_reg0[, all(!has_char(caption))])
  tlf_reg0[, caption := mk_captions(.SD), .I]

  # Add the manual outputs to registry:
  tlf_reg <- rbind(tlf_reg1, tlf_reg0, fill=TRUE)
  
  # Order by row_id within estimand:
  setorder(tlf_reg, estimand, row_id)
  tlf_reg[, estimand := as.character(estimand)]
  tlf_reg[, row_id := NULL]
  
  # Enable cross-referencing in books by endpoint-id tags:
  stopifnot(tlf_reg[, all(has_char(caption))])
  tlf_reg[, caption := paste0(caption, " {#sec-", endpoint_id, "}")]
  
  # Re-order columns:
  books <- getgrep("^book", names(tlf_reg))
  col_nms <- 
    c(books, "chapter", "outcome_var", "data_variable", "data_part", 
      "variable_label", "subgrp_var", "subgrp_label",
      "outcome_num", "data_name", "outcome_kind", "oc_kind_label", 
      "outcome_type", "outcome_label", "endpoint_num", "endpoint_id",  
      "tlf_type", "estimand", "subjects", "visit", "transformation",
      "role", "analysis", "imputation", "endpoint_level", 
      "analysis_label", "imputation_label", "folder", "file_stem",
      "section", "subsection", "caption", "footer")
  stopifnot(setequal(col_nms, names(tlf_reg)))  # setDiff(col_nms, names(tlf_reg))
  setcolorder(tlf_reg, col_nms)
  
  # Return
  tlf_reg[]
}

#' Make stem of file name for an auto-generated table or figure 
#'
#' @param regi a single-row `data.table`` with metadata
#'
#' @returns
#'
#' @examples
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[100]
#' mk_file_stem(regi)
#' 
#' ii <- tlf_reg[, which(nchar(subgrp_var) > 0)]
#' regi <- tlf_reg[ii[1]]
#' mk_file_stem(regi)
mk_file_stem <- function(regi) {
  parts <- regi[, c(
    endpoint_id,
    outcome_var,
    if(has_char(data_name)) data_name else NULL,
    if(has_char(analysis)) analysis else NULL,
    if(has_char(imputation)) imputation else NULL,
    if(is.numeric(visit) && !is.na(visit) && visit < 100) 
      paste0("day", visit) else NULL,
    if(has_char(subgrp_var)) subgrp_var else NULL,
    if(has_char(transformation)) transformation else NULL
  )]
  
  # Return pasted string:
  paste(parts, collapse = "_")
}


#' Save metadata to `metadata.xlsx`
#'
#' @param tlf_reg the results of `get_metadata()` (or `NULL`)
#' @param keep_intermediates also keep and save intermediate variables? (logical 
#' scalar)
#' @param out_dir directory to write `metadata.xlsx` to.
#'
#' @returns `NULL` invisibly.
#'
#' @examples
#' 
#' save_metadata()
#' 
save_metadata <- function(tlf_reg = NULL,
                          keep_intermediates = FALSE,
                          out_dir = "02_data_derived") {
  if(is.null(tlf_reg))
    tlf_reg <- get_metadata()
  stopifnot(is.data.table(tlf_reg),
            dir.exists(out_dir),
            requireNamespace("writexl", quietly = TRUE))
  writexl::write_xlsx(tlf_reg, path = paste0(out_dir, "/metadata.xlsx"))
  invisible(NULL)
}


#' Make captions for all tables and figures
#'
#' @param tlf_reg a metadata row (cf. `get_metadata()`)
#'
#' @returns vector of captions (strings) of the same length as the number of 
#' rows in `tlf_reg`
#'
#' @examples
#' 
#' tlf_reg <- get_metadata()
#' mk_captions(tlf_reg[31])
#' 
#' tlf_reg[140:142, .(Title = mk_captions(.SD)), .I]
#' 
mk_captions <- function(regi) {
  
  out_lab      <- regi[, outcome_label]
  trans        <- if(grepl("log", regi[, transformation])) "log" else ""
  subgrp_label <- regi[, subgrp_label]
  
  if(regi[, analysis] %in% c("summary", "histogram")) {
    res <- regi[, paste(out_lab, "-", analysis_label)]
    if(has_char(trans)) res <- paste(res, "-", trans, "transformed")
    return(res)
  }
  
  if(regi[, analysis] %in% "chgbl-time") {
    return(regi[, paste(out_lab, "over time - Estimated means")])
  }
  
  if(grepl("tipping", regi[, analysis]) & regi[, tlf_type == "table"]) {
    return(regi[, paste(out_lab, "-", analysis_label, 
                        "- Sensitivity analyses of primary and key secondary endpoints - Overview")])
  }
  
  do_analysis <- !regi[, grepl("responder|remission|sustain", analysis) || 
                         is.na(analysis_label) || nchar(analysis_label) == 0]
  ana_txt <- if(!has_char(subgrp_label)) " analysis of " else 
    paste(" analysis by", tolower_safe(subgrp_label), "at baseline of ")
  do_imputation <- regi[, has_char(imputation)]
  Role <- if(regi[, grepl("[tT]ipping", role)]) "Sensitivity" else regi[, role]
  res <- regi[, paste0(out_lab, " to day ", visit, " - ", 
                       if(do_analysis) paste0(analysis_label, " - ") else "",
                       if(do_imputation) paste0(imputation_label, " - ") else "",
                       Role, ana_txt, tolower(endpoint_level), " endpoint",
                       if(has_char(trans)) paste0(" - ", trans, " scale analysis") else "")]
  res
}


tolower_safe <- function(x) { # all caps abbreviations are not lowered
  if(all(x == toupper(x))) x else tolower(x)
}
