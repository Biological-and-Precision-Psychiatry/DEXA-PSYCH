
#' Generate tables and figures for TLF-book chapter
#' 
#' - All auto-generated tables and figures are produced
#' - The chapter qmd-file is generated from metadata and may include tables and 
#'   figures that are not auto-generated.
#'
#' @param chapter string - see `get_metadata()[, .N, chapter]` for options
#' @param estimands vector of estimands - one or more of `c("policy",
#'   "efficacy", "safety")`
#' @param set `"auto"` selects `"FAS"` for policy and efficacy estimands and
#'   `"SAS"` for safety
#' @param tlf_reg `get_metadata()` (to save computation time)
#' @param verbose print information about progress? (logical scalar)
#'
#' @returns `NULL` invisibly but saves the `qmd` chapter file and all 
#' auto-generated tables and figures to disk.
#'
#' @examples
#' 
#' # A short and quick chapter:
#' mk_tlf_ch("mdi_total", estimands = "policy")
#' 
#' # Multiple estimands:
#' mk_tlf_ch("mdi_total", estimands = c("policy", "efficacy"))
#' 
mk_tlf_ch <- function(chapter, 
                      estimands, 
                      set     = c("auto", "FAS", "SAS"), 
                      tlf_reg = NULL,
                      verbose = TRUE) {
  if(is.null(tlf_reg)) tlf_reg <- get_metadata()
  stopifnot(is.character(chapter) && length(chapter) == 1L, 
            chapter %in% tlf_reg[, chapter])
  stopifnot(is.logical(verbose) && length(verbose) == 1L,
            is.character(estimands) && all(estimands %in% c("policy", "efficacy", "safety")))
  Set <- set <- match.arg(set)
  Chapter <- chapter
  if(verbose) message("---- ", Chapter, " ----")
  
  for(Estimand in estimands) { # Estimand <- "policy"
    if(verbose) message("Now processing estimand: ", Estimand)
    if(set == "auto") Set <- if(Estimand == "safety") "SAS" else "FAS"
    
    # Tables:
    outc_vars <- tlf_reg[Chapter == chapter & tlf_type == "table", 
                         unique(outcome_var)]
    if(length(outc_vars) > 0) for(outc_var in outc_vars) {
      save_all_dt_tables(outc_var = outc_var, 
                         set      = Set,
                         estimand = Estimand, 
                         tlf_reg  = tlf_reg)
      save_all_gt_tables(outc_var = outc_var, 
                         estimand = Estimand, 
                         tlf_reg  = tlf_reg, 
                         verbose  = verbose)
    }
    
    # Figures:
    outc_vars <- tlf_reg[Chapter == chapter & tlf_type == "figure", 
                         unique(outcome_var)]
    if(length(outc_vars) > 0) for(outc_var in outc_vars) {
      save_all_figures(outc_var = outc_var, 
                       set      = Set,
                       estimand = Estimand, 
                       tlf_reg  = tlf_reg, 
                       verbose  = verbose)
    }
  }
  invisible(NULL)  
}

#' Make qmd-files for a quarto book (profile)
#'
#' @param book Name of the book/profile, see columns named `book_<profile>` in 
#' metadata (`tlf_registry.csv`). Valid `book` values are the values of 
#' `<profile>` (currently `"pp"` and `"ctr"`) in addition to `"all"`.
#' @param tlf_reg `get_metadata()` (to save computation time)
#' @param verbose 
#'
#' @returns Saves relevant `.qmd` chapter files to 
#' `paste0("40_tlf_source/generated-", book)` and returns `NULL` invisibly.
#'
#' @examples
#' 
#' mk_book_qmds("pp")
#' mk_book_qmds("all")
#' 
mk_book_qmds <- function(book    = "all",
                         tlf_reg = NULL, 
                         verbose = TRUE) { # book <- "pp"
  stopifnot(is.character(book) && length(book) == 1L)
  
  # Get metadata relevant the 'book':
  if(is.null(tlf_reg)) tlf_reg <- get_metadata()
  book_names <- getgrep("^book", names(tlf_reg)) 
  book_names <- c("all", gsub("^book_", "", book_names))
  if(!book %in% book_names) 
    stop("'book = ", book, "', but must be one of: ", paste(book_names, collapse = ", "))
  book_col <- paste0("book_", book)
  book_reg <- if(book == "all") tlf_reg else 
    tlf_reg[get(book_col) == 1 & role != "Internal"]

  # Make qmd-file For all combinations of estimand and chapter: 
  cases <- book_reg[, .N, .(estimand, chapter)]
  tmp <- lapply(seq_len(cases[, .N]), function(i) {
    mk_chapter_qmd(chapter  = cases[i, chapter],
                   estimand = cases[i, estimand],
                   out_dir  = paste0("40_tlf_source/generated-", book),
                   tlf_reg  = book_reg,
                   verbose  = verbose)
    # invisible(NULL)
  })

  invisible(NULL)
}

#' Make quarto yml-file for a book specified by metadata.
#'
#' @param book 
#' @param output_dir 
#' @param title 
#' @param author 
#' @param tlf_reg 
#' @param date 
#'
#' @returns saves a `yml` profile file to 
#' `paste0("40_tlf_source/_quarto-", book, ".yml")`.
#'
#' @examples
#' 
#' mk_book_yml("all")
#' mk_book_yml("pp", subtitle = "Primary Publication")
#' 
mk_book_yml <- function(book       = "all", 
                        output_dir = paste0("60_tlf_book_", book),
                        title      = "DEXA-PSYCH---Tables, Figures, and Listings",
                        subtitle   = NULL,
                        author     =  list("Rune Haubo B Christensen &", 
                                           "Eva Nina Sparre Wandall", 
                                           "for the DEXA-PSYCH Team"),
                        date       = "last-modified",
                        tlf_reg    = NULL
                        ) {
  
  # book <- "pp"
  # output_dir <- "61_tlf_book_pp"
  # title <- "Tables, Figures, and Listings - Primary Publication"
  # author <- "The DEXA-PSYCH stats team"
  # date <- "last-modified"
  # qmd_dir <- "generated-pp"
  # pre_chapters <- c("preface-pp.qmd") # , "intro-pp.qmd")
  
  # Helper-function: Get lines for quarto book 'part's:
  get_part_lines <- function(Estimand) { # Estimand <- "efficacy"
    if(!Estimand %in% book_reg[, unique(estimand)]) return(NULL)
    
    part_title <- switch (Estimand,
                          "intro"    = "TRIAL CONDUCT",
                          "overview" = "OVERVIEW",
                          "policy"   = "POLICY ESTIMAND",
                          "efficacy" = "EFFICACY ESTIMAND",
                          "safety"   = "SAFETY")
    chapters <- book_reg[estimand == Estimand, unique(chapter)]
    qmd_stems <- sapply(chapters, function(ch) mk_qmd_name(Estimand, ch))
    
    part_lines <- 
      c(paste0("- part: \"", part_title, "\""),
        "  chapters:",
        paste0("    - ", qmd_dir, "/", qmd_stems))
    paste0("    ", part_lines)
  }

  # Check arguments:
  stopifnot(is.character(book) && length(book) == 1L,
            is.character(output_dir) && length(output_dir) == 1L,
            is.character(title) && length(title) == 1L,
            (is.character(subtitle) && length(subtitle) == 1L) || 
              is.null(subtitle),
            is.list(author) && length(author) >= 1L && 
              all(sapply(author, is.character)) && all(sapply(author, length) == 1),
            is.character(date) && length(date) == 1L)
  
  # Get metadata relevant the 'book':
  if(is.null(tlf_reg)) tlf_reg <- get_metadata()
  book_names <- getgrep("^book", names(tlf_reg)) 
  book_names <- c("all", gsub("^book_", "", book_names))
  if(!book %in% book_names) 
    stop("'book = ", book, "', but must be one of: ", paste(book_names, collapse = ", "))
  book_col <- paste0("book_", book)
  book_reg <- if(book == "all") tlf_reg else tlf_reg[get(book_col) == 1]
  qmd_dir <- paste0("generated-", book)
  
  valid_estimands <- c("overview", "intro", "policy", "efficacy", "safety")
  stopifnot(all(book_reg[, unique(estimand)] %in% valid_estimands))

  # Make lines for the first part of the book:  
  book_lines <- 
    c("project:",
      paste0("  output-dir: ../", output_dir),
      "",
      "book:",
      paste0("  title: \"", title, "\""),
      if(is.null(subtitle)) NULL else paste0("  subtitle: \"", subtitle, "\""),
      "  author:",
      sapply(author, function(a) paste0("    - name: \"", a, "\"")),
      paste0("  date: ", date),
      "  page-footer:",
      paste0("    left: \"Copyright ", year(Sys.Date()), 
             ", the DEXA-PSYCH Team\""),
      "    right:",
      "      - icon: github",
      "        href: https://github.com/Biological-and-Precision-Psychiatry",
      "      - icon: linkedin",
      "        href: https://dk.linkedin.com/company/copenhagen-biological-and-precision-psychiatry",
      "  chapters:", 
      "    - index.qmd")

  # Make lines for each "part":
  part_lines <- lapply(valid_estimands, get_part_lines) |> unlist()

  # Collect all lines:  
  lines <- c(book_lines, part_lines)
  
  # Write 'lines' to quarto-profile-yml file:
  writeLines(lines, con = paste0("40_tlf_source/_quarto-", book, ".yml"))
  # writeLines(lines) # inspect
  
  invisible(lines)
}


#' Generate a chapter-level .qmd file from TLF metadata
#'
#' @param chapter string - see `get_metadata()[, .N, chapter]` for options
#' @param estimand 
#' @param tlf_reg `get_metadata()` (to save computation time)
#' @param out_dir path to `qmd`-file produced
#' @param verbose Print progress? (logical scalar)
#' @param knitr_block 
#'
#' @returns saves the `qmd`-file to disk. Also returns the vector of strings 
#' that make up the lines of the `qmd`-file.
#'
#' @examples
#' 
#' # Generate a chapter `qmd`-file for the TLF-book
#' mk_chapter_qmd(verbose = TRUE)
#' 
#' # Another example:
#' res <- mk_chapter_qmd("mdi_total", estimand = "efficacy", verbose = TRUE)
#' cat(res[1:20], sep = "\n")
#' 
mk_chapter_qmd <- function(
    chapter  = "madrs_total10", 
    estimand = c("policy", "efficacy", "safety", "intro", "overview"),
    tlf_reg  = NULL,
    out_dir  = "40_tlf_source/generated", # set to NULL for no writing to disk
    verbose  = FALSE,
    knitr_block = FALSE) {
  # Chapter = "biochemistry"; Estimand <- "safety"
  
  Chapter <- chapter
  Estimand <- match.arg(estimand)
  if(is.null(tlf_reg)) tlf_reg <- get_metadata()
  stopifnot(is.character(chapter) && length(chapter) == 1L,
            is.data.table(tlf_reg),
            chapter %in% tlf_reg[, chapter],
            is.logical(verbose) && length(verbose) == 1L,
            is.null(out_dir) || (is.character(out_dir) && length(out_dir) == 1L))
  
  regi_all <- tlf_reg[Chapter == chapter & estimand == Estimand]
  chapter_name <- Chapter
  if(regi_all[1L, chapter == outcome_var]) 
    chapter_name <- regi_all[1L, variable_label]
  if(!grepl("-", chapter_name)) chapter_name <- tools::toTitleCase(chapter_name)
  
  # Make qmd lines:
  # YAML front matter
  lines <- c(
    "---",
    paste0('title: "', chapter_name, '"'),
    "---",
    "")
  
  # Loop over all rows of regi_all to add lines:
  for (i in seq_len(nrow(regi_all))) { # i <- 27
    regi <- regi_all[i]
    
    # Add section:
    if(!regi[, is.na(section) || (is.character(section) & nchar(section) == 0L)])
      lines <- c(lines, paste0("## ", regi[, section]), "")
    
    # Add subsection:
    if(!regi[, is.na(subsection) || (is.character(subsection) & nchar(subsection) == 0L)])
      lines <- c(lines, paste0("### ", regi[, subsection]), "")
    
    # Add Table title:
    lines <- c(lines, paste0("#### ", regi[, caption]), "")

    # Add include block:
    lines <- c(lines, mk_include_block(regi, 
                                       absolute_path = TRUE, 
                                       knitr_block   = knitr_block), 
               "")
  }
  
  # Write to disk:
  if(!is.null(out_dir)) {
    if(!dir.exists(out_dir)) dir.create(out_dir)
    out_path <- file.path(out_dir, mk_qmd_name(Estimand, chapter))
    writeLines(lines, con = out_path)
    if(verbose)
      message(sprintf("Written: %s  (%d lines)", out_path, length(lines)))
  }
  
  invisible(lines)
}

#' Generate name of qmd chapter files
#'
#' @param estimand 
#' @param chapter 
#'
#' @returns name of qmd-file
#'
#' @examples
#' 
#' mk_qmd_name("intro", "participant status")
#' mk_qmd_name("policy", "madrs_total10")
#' 
mk_qmd_name <- function(estimand, chapter) {
  paste0(estimand, "_", gsub("\\s", "_", chapter), ".qmd")
}

#' Make qmd-lines for a table or figure
#'
#' @param regi a one-row metadata `data.table` from `get_metadata()`
#' @param absolute_path absolute or relative path to table or figure?
#' @param knitr_block use knitr-type include block instead of standard Quarto
#' style include block?
#'
#' @returns qmd-lines for a table or figure
#'
#' @examples
#' 
#' # Get metadata line:
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[role == "Primary" & endpoint_level == "Primary"][1]
#' 
#' # Include-block with relative paths
#' mk_include_block(regi)
#' 
#' # Include-block with absolute paths
#' mk_include_block(regi, absolute_path = TRUE)
#' 
#' # knitr-type include-block 
#' mk_include_block(regi, knitr_block = TRUE)
#' 
mk_include_block <- function(regi, 
                             absolute_path = FALSE,
                             knitr_block   = FALSE) {
  tlf_type <- regi[, tlf_type]
  folder   <- regi[, folder]
  stem     <- regi[, file_stem]
  
  if(tlf_type == "table") {
    lines <- if(knitr_block) {
      mk_table_lines2(stem, folder, absolute_path) 
    } else {
      mk_table_lines(stem, folder, absolute_path)
    }
  } else if(tlf_type == "figure") {
    lines <- c(mk_figure_lines(stem, folder, absolute_path = FALSE),
               mk_footer_lines(regi))
  } else lines <- character(0L)
  
  lines
}

#' Standard table-lines for include block
#'
#' @param stem 
#' @param folder 
#' @param absolute_path 
#'
#' @returns
#' @examples
#' 
#' # Get metadata line:
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[role == "Primary" & endpoint_level == "Primary"][1]
#' 
#' # Standard include block:
#' mk_table_lines(regi[, file_stem], regi[, folder])
#' 
mk_table_lines <- function(stem, folder, absolute_path = FALSE) {
  mk_tbl_lines <- function(type = "html", path) {
    c(paste0("::: {.content-visible when-format=\"", type, "\"}"),
      paste0("{{< include ", path, " >}}"),
      ":::",
      "")
  }

  paths <- list(html = file.path(folder, paste0(stem, ".html")),
                pdf  = file.path(folder, paste0(stem, ".tex")))
  # if(!all(sapply(paths, file.exists))) return(character(0L))
  if(absolute_path) paths <- lapply(paths, normalizePath, mustWork = FALSE)
  # NOTE: Not all 'paths' exist when the qmd is generated using this function.
  #  This means that we have to either 'return(character(0L))' in which case 
  #  Quarto can process the qmd, or use 'normalizePath(..., mustWork = FALSE)'
  #  which means that a qmd-file is made that refers to table-files that may
  #  not exist and running Quarto errors. Tipping point overview tables are 
  #  first made _after_ the qmd is generated, for example.
  
  c("", mk_tbl_lines("html", paths[[1]]),
    mk_tbl_lines("pdf", paths[[2]]))
}



#' Knitr-type table-lines for qmd include block
#'
#' Make table-lines using a knitr-block This was previously used for AE tables
#' because the include-block with ::: -fencing did not work there (for unknown
#' reasons).
#' 
#' @param stem 
#' @param folder 
#' @param absolute_path 
#'
#' @returns
#' @examples
#' 
#' # Get metadata line:
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[role == "Primary" & endpoint_level == "Primary"][1]
#' 
#' # Standard include block:
#' mk_table_lines2(regi[, file_stem], regi[, folder])
#' 
mk_table_lines2 <- function(stem, folder, absolute_path = FALSE) {
  paths <- list(html = file.path(folder, paste0(stem, ".html")),
                pdf  = file.path(folder, paste0(stem, ".tex")))
  if(!all(sapply(paths, file.exists))) return(character(0L))
  if(absolute_path) paths <- lapply(paths, normalizePath, winslash = "/")

  c("",
    "```{r echo=FALSE, results='asis'}",
    "if (knitr::is_html_output()) {",
    paste0("  cat(readLines('", paths$html, "'), sep = '\\n')"),
    "} else if (knitr::is_latex_output()) {",
    paste0("  cat(readLines('", paths$pdf, "'), sep = '\\n')"),
    "}",
    "```")  
}

#' Make figure-lines for qmd include block
#'
#' @param stem 
#' @param folder 
#' @param absolute_path 
#'
#' @returns
#' @examples
#' 
#' # Get metadata line:
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[analysis == "chgbl-time"][1]
#' 
#' # Standard include block:
#' mk_figure_lines(regi[, file_stem], regi[, folder])
#' 
mk_figure_lines <- function(stem, folder, absolute_path = FALSE) {
  path <- if(absolute_path) normalizePath(paste0(folder, "/", stem, ".png")) else
    paste0(gsub("^.*/", "../", folder), "/", stem, ".png")
  path <- gsub("\\.png$", "", path)
  
  c("", paste0("![](", path, "){width=98% fig-align=\"center\"}"), "")
}


#' Make footer lines for figures
#'
#' @param regi 
#'
#' @returns
#' @examples
#' 
#' # Get metadata line:
#' tlf_reg <- get_metadata()
#' regi <- tlf_reg[tlf_type == "figure" & endpoint_level == "Primary"][1]
#' 
#' # Standard include block:
#' regi <- tlf_reg[endpoint_id == "E02-26"]
#' mk_footer_lines(regi)
#' 
mk_footer_lines <- function(regi) {
  f_cref <- get_crossref_line(regi[, endpoint_id])
  f_list <- mk_footer_list(regi)
  f_list <- f_list[!sapply(f_list, is.null)]
  
  # Remove entries for duplicated topics (eg. CRP):
  topics <- gsub(":.*$", "", unlist(f_list))
  f_list <- f_list[!duplicated(topics)]
  
  flines <- c(unlist(f_list), f_cref)
  flines <- sapply(flines, function(fl) paste0(" - ", fl)) |> unname()
  if (length(flines) == 0) flines <- character(0)
  
  c(paste0("::: {style=\"font-size: 70%;\"}"),
    "",
    flines,
    "",
    ":::",
    "")
}
