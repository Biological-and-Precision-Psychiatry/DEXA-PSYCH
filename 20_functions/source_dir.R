source_dir <- function(path) {
  R_file_names <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE)
  invisible(lapply(R_file_names, source))
}
