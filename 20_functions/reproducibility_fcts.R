# reproducibility_fcts.R

#' Compare rds files to a reference
#' 
#' Compare a new version of rds files with a reference to assess 
#' reproducibility. Uses `waldo::compare()` for fully nested comparison.
#'
#' @param reference_path path to reference version, typically a repo root
#' @param new_path path to new version, typically the current repo
#' @param folder sub-folder on each path containing all html-files.
#' @param tolerance numerical tolerance, eg, `1e-6`. Use `NULL` for greatest 
#' sensitivity to differences.
#' @param copy_files copy reference files to a local folder? Typically 
#' reference_path points to a network drive where copy before reading is faster
#' than reading directly.
#' @param verbose print progress?
#'
#' @returns a list with `summary`, `table_diffs` and `footer_diffs`.
#' 
#' @examples 
compare_rds_files <- function(
    reference_path,                 # path to ref repo root
    new_path   = ".",               # path to new repo root
    folder     = "tables",          # folder in repo with rds files
    tolerance  = NULL,
    copy_files = TRUE,
    verbose    = TRUE) {
  stopifnot(dir.exists(reference_path),
            dir.exists(new_path),
            dir.exists(paste0(reference_path, "/", folder)),
            dir.exists(paste0(new_path, "/", folder)))
  
  # Get dir's and file names:
  old_dir <- paste0(reference_path, "/", folder)
  new_dir <- paste0(new_path, "/", folder)
  old_fns <- list.files(old_dir, pattern = "\\.[rR]ds$", full.names = FALSE)
  new_fns <- list.files(new_dir, pattern = "\\.[rR]ds$", full.names = FALSE)
  
  # Initiate summary table:
  .d <- data.table(file  = union(new_fns, old_fns), 
                   where = "both")
  .d[!file %in% old_fns, where := "only new"]
  .d[!file %in% new_fns, where := "only ref"]
  both_fns <- .d[where == "both", file]
  
  if(length(both_fns) == 0)
    stop("No files exist on both reference and new paths.")
  
  if(verbose) {
    message("Found ", .d[, .N], " files in '", folder, 
            "' across 'reference' and 'new' paths.")
    message(" - File distribution: Both (", length(both_fns), 
            "), only new (", .d[, sum(where == "only new")], 
            "), only ref (", .d[, sum(where == "only ref")], ").")
  }
  
  # Optionally copy files to a temp dir:  
  ref_dir <- old_dir
  if(copy_files) {
    tmp_dir <- tempfile()
    dir.create(tmp_dir, recursive = TRUE)
    on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
    if(verbose) message(" - Copying ", length(both_fns), " files to temp dir...")
    ok <- file.copy(file.path(old_dir, both_fns), tmp_dir, overwrite = TRUE)
    if(all(ok)) ref_dir <- tmp_dir else 
      warning("Failed to copy files to temp folder. Reading from reference path instead.")
  }
  
  # Compare all common files:
  if(verbose) message(" - Comparing ", length(both_fns), " files...")
  
  res_list <- lapply(setNames(both_fns, both_fns), function(fn) { # fn <- both_fns[1]
    tryCatch({
      old <- readRDS(paste0(ref_dir, "/", fn))
      new <- readRDS(paste0(new_dir, "/", fn))
      waldo::compare(old, new, tolerance = tolerance)
    }, error = function(e) {
      paste("Error reading/comparing file:", conditionMessage(e))
    })
  })
  
  # Indicate in summary the number of diffs found:
  .d[where == "both", n_diffs  := sapply(res_list, length)]
  if(verbose) {
    message(" - ", .d[where == "both", sum(n_diffs > 0)], " files had changed.")
    message(" - Done.")
  }  
  
  # Return:
  list(summary = .d[], diffs = res_list)
}


#' Compare html tables to a reference
#' 
#' Compare a new version of html tables with a reference to assess 
#' reproducibility. Table contents and footers are each compared.
#' Uses `waldo::compare()` for fully nested comparison.
#'
#' @param reference_path path to reference version, typically a repo root
#' @param new_path path to new version, typically the current repo
#' @param folder sub-folder on each path containing all html-files.
#' @param copy_files copy reference files to a local folder? Typically 
#' reference_path points to a network drive where copy before reading is faster
#' than reading directly.
#' @param verbose print progress?
#'
#' @returns a list with `summary`, `table_diffs` and `footer_diffs`.
#'
#' @examples
compare_html_tables <- function(
    reference_path,                 # path to ref repo root
    new_path   = ".",               # path to new repo root
    folder     = "tables",          # folder in in repo with html files
    copy_files = TRUE,
    verbose    = TRUE) {
  stopifnot(dir.exists(reference_path),
            dir.exists(new_path),
            dir.exists(paste0(reference_path, "/", folder)),
            dir.exists(paste0(new_path, "/", folder)))
  
  # Get dir's and file names:
  old_dir <- paste0(reference_path, "/", folder)
  new_dir <- paste0(new_path, "/", folder)
  old_fns <- list.files(old_dir, pattern = "\\.html$", full.names = FALSE)
  new_fns <- list.files(new_dir, pattern = "\\.html$", full.names = FALSE)
  
  # Initiate summary table:
  .d <- data.table(file  = union(new_fns, old_fns), 
                   where = "both")
  .d[!file %in% old_fns, where := "only new"]
  .d[!file %in% new_fns, where := "only ref"]
  both_fns <- .d[where == "both", file]
  
  if(length(both_fns) == 0)
    stop("No files exist on both reference and new paths.")
  
  if(verbose) {
    message("Found ", .d[, .N], " files in '", folder, 
            "' across 'reference' and 'new' paths.")
    message(" - File distribution: Both (", length(both_fns), 
            "), only new (", .d[, sum(where == "only new")], 
            "), only ref (", .d[, sum(where == "only ref")], ").")
  }
  
  # Optionally copy files to a temp dir:  
  ref_dir <- old_dir
  if(copy_files) {
    tmp_dir <- tempfile()
    dir.create(tmp_dir, recursive = TRUE)
    on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
    if(verbose) message(" - Copying ", length(both_fns), " files to temp dir...")
    ok <- file.copy(file.path(old_dir, both_fns), tmp_dir, overwrite = TRUE)
    if(all(ok)) ref_dir <- tmp_dir else 
      warning("Failed to copy files to temp folder. Reading from reference path instead.")
  }
  
  # Compare all common files:
  if(verbose) message(" - Comparing ", length(both_fns), " files...")
  
  res_list <- lapply(setNames(both_fns, both_fns), function(fn) { # i <- 1
    tryCatch({
      old <- get_html_gt_tab(paste0(ref_dir, "/", fn))
      new <- get_html_gt_tab(paste0(new_dir, "/", fn))
      list(footer = waldo::compare(old$footer, new$footer),
           table  = waldo::compare(old$table, new$table))
    }, error = function(e) {
      msg <- paste("Error reading/comparing file:", conditionMessage(e))
      list(table = msg, footer = msg)
    })
  })
  
  # Separate into lists for tables and footers:
  res_table  <- lapply(res_list, "[[", "table")
  res_footer <- lapply(res_list, "[[", "footer")
  
  # Indicate in summary the number of diffs found:
  .d[where == "both", table_diffs  := sapply(res_table, length)]
  .d[where == "both", footer_diffs := sapply(res_footer, length)]
  
  if(verbose) {
    message(" - ", .d[where == "both", sum(table_diffs > 0)], " tables changed.")
    message(" - ", .d[where == "both", sum(footer_diffs > 0)], " footers changed.")
    message(" - Done.")
  }  
  
  # Return:
  list(summary = .d[], table_diffs = res_table, footer_diffs = res_footer)
}

#' Extract table and footer from an html file
#'
#' Reads html tables produced by `gt::gt()` with footers set by 
#' `gt::tab_source_note()`.
#'
#' @param file full path to html file
#' @param convert if `FALSE` (the default) contents are not converted from 
#' character to integer, numeric etc.
#'
#' @returns a list with `table` and `footer` elements.
#'
#' @examples
get_html_gt_tab <- function(file, convert = FALSE) {
  # Read html file:
  doc <- read_html(file, encoding = "UTF-8")
  
  # Get footer:
  footer <- html_elements(doc, "tfoot td") |>
    html_text2()
  
  # Remove gt-footer:
  xml2::xml_remove(html_elements(doc, "tfoot"))
  
  # Return table and footer:
  list(table  = html_table(html_element(doc, "table"), convert = convert),
       footer = footer)
}


