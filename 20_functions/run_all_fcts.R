
# clear 
# - derived data
# - output_db
# - tables, figures, qmd
# - html reports

# 02_data_derived/
#   all files 
# 03_checksums/.
# 40_tlf_source/
#   .quarto
#   auto_figures
#   auto_tables
#   figures
#   tables
#   generated
#   _freeze
#   
# 50_output_db
# 60_tlf_book-*/.

clear_all <- function(remove                = TRUE,
                      rm_html               = TRUE,
                      rm_cache              = FALSE,
                      rm_checksums          = FALSE) {
  rm_checksums <- remove && rm_checksums
  rm_html      <- remove && rm_html
  rm_cache     <- remove && rm_cache
  res1 <- clear_data_derived(remove = remove)
  res3 <- clear_checksums(remove = rm_checksums)
  res4 <- clear_outputs(remove = remove)
  res5 <- clear_output_db(remove = remove)
  res6 <- clear_tlf_book(remove = remove)
  res7 <- clear_html_pdf_res(remove = rm_html)
  res8 <- clear_par_prog_res(remove = remove)
  res9 <- clear_cache(remove = rm_cache)
  
  res <- rbind(res1, res3, res4, res5, res6, res7, res8, res9)
  res[, dir := dirname(files)]
  res[, files := basename(files)]
  setcolorder(res, c("dir", "files", "removed"))
  res[]
}
# clear_all(remove = FALSE)
# clear_all(remove = TRUE)

clear_html_pdf_res <- function(remove = TRUE, exclude = "^99") {

  # Consider files in dirs that start with "3" or "80":
  dirs <- list.dirs(path = ".", recursive = FALSE, full.names = FALSE)
  dirs <- getgrep("^3|^80", dirs)
  
  # Get paths to files to remove:
  files <- lapply(dirs, function(dir) {
    list.files(path = dir, 
               pattern = "\\.html$|\\.pdf$", 
               full.names = TRUE)
  }) |> unlist() |> unname()
  
  # Exclude files from draft reports:
  files <- files[!grepl(exclude, basename(files))]

  # Remove files and return  
  removed <- if(remove) file.remove(files) else FALSE
  return(data.table(files, removed))
}
# clear_html_res(remove = FALSE)
# clear_html_res(remove = TRUE)


clear_data_derived <- function(path    = "02_data_derived", 
                               pattern = "\\.[Rr]ds|\\.xlsx|\\.xls|\\.csv$",
                               remove  = TRUE) {
  files <- list.files(path         = path, 
                      pattern      = pattern,
                      include.dirs = FALSE,
                      full.names   = TRUE)
  removed <- if(remove) file.remove(files) else FALSE
  return(data.table(files, removed))
}
# clear_data_derived(remove = FALSE)

clear_checksums <- function(path    = "03_checksums", 
                            pattern = NULL,
                            remove  = TRUE) {
  files <- list.files(path         = path, 
                      pattern      = pattern,
                      include.dirs = FALSE,
                      full.names   = TRUE)
  removed <- if(remove) file.remove(files) else FALSE
  return(data.table(files, removed))
  
  removed <- unlink(path, recursive = TRUE)
  removed = if(removed == 0) TRUE else FALSE
  return(invisible(data.table(files = path, removed)))
}
# clear_checksums(remove = FALSE)


clear_outputs <- function(
    path       = "40_tlf_source",
    can_remove = c("^\\.quarto$", "^_freeze$", "^auto_figures$", "^auto_tables$", 
                   "^site_libs$", "^\\.gitignore$", "^figures$", 
                   "^generated-", "^tables$", "^_quarto-.+\\.yml$"),
    remove     = TRUE) { # remove <- FALSE
  
  do_remove <- list.files(path       = path, 
                          pattern    = paste(can_remove, collapse = "|"), 
                          full.names = TRUE,
                          all.files = TRUE)
  if(remove) {
    removed <- sapply(do_remove, unlink, recursive = TRUE)
  }
  if(!remove || length(removed) == 0)
    removed <- FALSE
  if(is.numeric(removed)) removed <- ifelse(removed == 0, TRUE, FALSE)
  return(data.table(files = do_remove, removed))
}
# file.create("40_tlf_source/.quarto")
# clear_outputs(remove = FALSE)
# clear_outputs(remove = TRUE)

clear_output_db <- function(path   = "50_output_db",
                            remove = TRUE) {
  if(remove) {
    removed <- unlink(path, recursive = TRUE)
    removed = if(removed == 0) TRUE else FALSE
  } else {
    removed <- FALSE
  }
  
  return(data.table(files = path, removed))
}
# clear_output_db(remove = FALSE)
# clear_output_db(remove = TRUE)

clear_cache <- function(path   = "05_cache",
                        remove = TRUE) {
  if(remove) {
    removed <- unlink(path, recursive = TRUE)
    removed = if(removed == 0) TRUE else FALSE
  } else {
    removed <- FALSE
  }
  
  return(data.table(files = path, removed))
}
# clear_cache(remove = FALSE)
# clear_cache(remove = TRUE)


clear_tlf_book <- function(path   = "60_tlf_book",
                           remove = TRUE) {
  dirs <- list.dirs(path = ".", recursive = FALSE, full.names = FALSE)
  paths <- getgrep(path, dirs)

  if(remove) {
    removed <- unlink(paths, recursive = TRUE)
    removed = if(removed == 0) TRUE else FALSE
  } else {
    removed <- FALSE
  }
  
  return(data.table(files = paths, removed))
}
# clear_tlf_book(remove = FALSE)
# clear_tlf_book(remove = TRUE)

clear_par_prog_res <- function(path = "80_parallel_programming/pp_outputs",
                               remove = TRUE) {
  do_remove <- list.files(path, full.names = TRUE)
  if(remove) {
    removed <- sapply(do_remove, unlink, recursive = TRUE)
  }
  if(!remove || length(removed) == 0)
    removed <- FALSE
  if(is.numeric(removed)) removed <- ifelse(removed == 0, TRUE, FALSE)
  return(data.table(files = do_remove, removed))
}

#' Render multiple R Markdown files, each in a fresh R session
#'
#' @description
#' Loops over a set of `.Rmd` files and renders each one with
#' `render_rmd()`, i.e. in its own fresh `Rscript` subprocess. This mirrors
#' the behaviour of repeatedly pressing RStudio's "Knit" button and avoids
#' environment/scoping issues that can occur when rendering many files via
#' `rmarkdown::render()` within a single shared R session.
#'
#' @param filenames Character vector of `.Rmd` file names (not full paths)
#'   to render, e.g. `c("01_intro.Rmd", "02_analysis.Rmd")`. 
#' @param path Character scalar giving the directory containing the files
#'   in `filenames`. 
#' @param ... passed on to `render_rmd()`.
#' @param rscript see `render_rmd()`.
#' @param verbose print progress?
#'
#' @returns `NULL` invisibly.
#'
#' @examples
#'
#' # Process a couple of files:
#' filenames <- c("01_flowchart.Rmd", "02_baseline_table.Rmd")
#' path <- "32_manual_analyses"
#' render_multiple_rmd(filenames, path = path)
#' 
render_multiple_rmd <- function(filenames,
                                path    = ".",
                                ...,
                                rscript = "Rscript",
                                verbose = TRUE) {
  # Check arguments:
  stopifnot(is.character(filenames) && length(filenames) >= 1L,
            is.character(path) && length(path) == 1L && dir.exists(path),
            is.character(rscript) && length(rscript) == 1L && !is.na(rscript),
            is.logical(verbose) && length(verbose) == 1L && !is.na(verbose))
  
  # Make full paths to files:
  full_paths <- file.path(path, filenames)
  
  # Check that all files exist:
  missing_files <- filenames[!file.exists(full_paths)]
  if(length(missing_files) > 0L) {
    stop("The following file(s) were not found in '", path, "':\n  ",
         paste(missing_files, collapse = "\n  "))
  }
  
  # Check that all files are rmd or Rmd:
  not_rmd <- filenames[!grepl("\\.[Rr]md$", filenames)]
  if(length(not_rmd) > 0L) {
    warning("The following file(s) do not have an .Rmd/.rmd extension:\n  ",
            paste(not_rmd, collapse = "\n  "), call. = FALSE)
  }
  
  # Process each Rmd-file in turn:
  n <- length(filenames)
  for(i in seq_len(n)) { # i <- 1
    if(verbose) 
      message("[", i, "/", n, "] Rendering '", filenames[i], "' ... ")
    res <- render_rmd(full_paths[i], ..., rscript = rscript)
  }
  
  message("\nAll ", n, " file(s) rendered successfully.")
  
  invisible(NULL)
}


#' Render a single R Markdown file in a fresh R session
#'
#' @description
#' Renders an `.Rmd` file by launching a new, separate R process via
#' `system2()` and calling `rmarkdown::render()`e. This mirrors the
#' behaviour of RStudio's "Knit" button.
#'
#' @param input Character scalar giving the path to the `.Rmd` file to
#'   render. 
#' @param ... Additional named arguments passed on to
#'   `rmarkdown::render()` (e.g. `output_format`,
#'   `output_dir`, `output_file`, `params`). 
#' @param rscript Character scalar giving the name or full path of the
#'   `Rscript` executable to use for the subprocess. Defaults to
#'   `"Rscript"`.
#'
#' @returns potential `status` invisibly. 
#'
#' @examples
#'
#' # Render an Rmd-file:
#' render_rmd(input = "32_manual_analyses/01_flowchart.Rmd")#' 
#' 
#' # This is an error:
#' # render_rmd(input = "32_manual_analyses/01_flowchart.Rmd", quiet = FALSE)
#' 
#' # Include extra parameters:
#' render_rmd(input  = "32_manual_analyses/01_flowchart.Rmd", 
#'            params = list(stop_on_error = TRUE))
#' 
render_rmd <- function(input, ..., rscript = "Rscript") {
  # Check arguments:
  stopifnot(is.character(input) && length(input) == 1L && !is.na(input),
            file.exists(input),
            isTRUE(grepl("\\.[Rr]md$", input)),
            is.character(rscript) && length(rscript) == 1L && !is.na(rscript),
            nchar(Sys.which(rscript)) > 0,
            !any(c("input", "quiet") %in% names(list(...))),
            requireNamespace("rmarkdown", quietly = TRUE))
  
  rscript_path <- Sys.which(rscript)
  extra_args <- list(...)
  
  # Collect arguments 
  render_args <- c(list(input = input, quiet = TRUE), extra_args)
  
  # Build R-call-string:
  sys_arg1 <- paste0("do.call(rmarkdown::render, ", deparse1(render_args), ")")
  
  # Build args-vector for `system2()`:
  sys_args <- c("-e", shQuote(sys_arg1))
  
  # Evaluate the Rmd-file:
  log <- system2(rscript_path, args = sys_args, stdout = TRUE, stderr = TRUE) |> 
    suppressWarnings() # to prevent errors being printed here.
  status <- attr(log, "status")
  # - 'log' holds potential error messages - warnings are ignored.
  # - 'status != 0' if an error occurred
  
  # Give error message if an error occurred:
  if(!is.null(status) && status != 0)
    stop("An error occurred while processing ", input, ":\n", 
         paste(log, collapse = "\n"))
  
  invisible(status)
}


