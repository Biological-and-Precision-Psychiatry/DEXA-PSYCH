## Display variables in DEXA data
##
## Description:
##
## Display which variables occur and where they occur in a DEXA data-list.
##
## Arguments: 
## - datalist    : A list of data.tables each containing a block of data corresponding
##   to each of the 11 the redcap event-names (one for each visit, one for events etc.)
## - names       : Vector of (partial) name of a variable to search for in DEXA data. 
## - print_empty : Print the names of blocks in which none of 'names' occur?
## - type        : Determines how 'names' is searched for. "begin" searches for 
##   variables that begin with 'names'; "end" searches for variables that end with 
##   "names"; 'exact' searches for variables exactly equal to "names" and; 'any' 
##   searches for any matches for "names" among variable names.
## 
## Return value:
## - Prints variables names and where they occur in the DEXA data.
## 
## Examples:
##
## show_variables(dl2, "outcome") # all variables that contain "outcome"
## show_variables(dl2, "madrs", type="begin") # all variables that begin with "madrs" 
## show_variables(dl2, "complete") # all completion flags
## show_variables(dl2, "complete", type="end") # all variables that end with "complete"
## 
show_variables <- function(datalist=dl2, names="rygning", print_empty=TRUE, 
                           type = c("any", "exact", "begin", "end")) { # datalist <- dl2
  ## Displays which parts of the data variables in 'names' occur.
  type <- match.arg(type)
  if(type %in% c("begin", "exact"))
    names <- paste0("^", names)
  if(type %in% c("end", "exact"))
    names <- paste0(names, "$")
  txt <- paste0(names, collapse = "|")
  lnms <- lapply(datalist, function(dd) getgrep(txt, names(dd)))
  for(i in seq_along(lnms)) {
    if(length(lnms[[i]]) > 0 | print_empty)
      cat(names(lnms)[i], "\n", sep="")
    if(length(lnms[[i]]))
      cat(strwrap(paste(lnms[[i]], collapse = " "), prefix ="  "), sep="\n")
  }
  invisible(lnms)
}


## Extract variables from DEXA raw data
##
## Description:
## Extract data on variables from DEXA raw data in a structured fashion.
## A dataset in long format is generated with columns for date, variable names, 
## value etc. See section Return value for further details.
##
## Arguments: 
## - datalist    : A list of data.tables each containing a block of data corresponding
##   to each of the 11 the redcap event-names (one for each visit, one for events etc.)
## - name       : (Partial) name of a variable in DEXA data. 
## - name_type  : Determines how 'names' is searched for. "begin" searches for 
##   variables that begin with 'names'; "end" searches for variables that end with 
##   "names"; 'exact' searches for variables exactly equal to "names" and; 'any' 
##   searches for any matches for "names" among variable names.
## - test_type : Require all variables to be of a specific type. "none" (default)
##   performs no test of type.
## - clean_part: Clean the resulting "part" variable?
## - test_part: Test if the resulting "part" variable is sensible?
## - guess_date: 
## - dato_default: Variables in which to search for date of assessment if none is found
##   specific to "name".
## - exclude_varibs: Vector of variables names to exclude from the structured extraction.
## - value_character: Force return value-variable to be of type character?
## 
## Return value:
## A data.table with the following columns:
## - ID: Participant ID
## - date: Date of assessment
## - date_var: Date-variable from which 'date' is taken
## - c_visit: The visit (character value)
## - orig: Name of the original variable from which the data is extracted (eg. "madrs_2_dag0")
## - variable: Name of the (eg. "madrs")
## - part: The component or part of the variable (eg. "2")
## - bsg_cmplt: Completion flag for besøg (value of "besgsafslutning_complete"; 
##     relevant for most visits)
## - ranmed_cmpl: Completion flag for randomization (value of 
##     "randomisering_og_medicinudlevering_complete"; only for day0 visit)
## - assess_cmpl: Completion flag for assessment (eg. "madrs_dag4_complete")
## - value: The value of the variable being extracted (if numeric)
## - c_value: The value of the variable being extracted (if character)
## - d_value: The value of the variable being extracted (if Date)
##  
## Examples: 
## 
## get_dexadata(dl2, name="madrs") # extract all data on madrs from DEXA data in dl2
## 
## 
get_dexadata <- function(datalist = dl2, name="madrs", 
                         name_type = c("any", "exact", "begin", "end"),
                         test_type = c("none", "numeric", "integer", "character", "factor"),
                         clean_part = name %in% c("madrs", "hamd", "hama", "saps", "mdi", "eq5d5l", "gti", "facitf"),
                         test_part = name %in% c("madrs", "hamd", "hama", "saps", "mdi", "eq5d5l", "gti"),
                         guess_date = TRUE,
                         dato_default = c("besoeg_dato", "screen_dato"),
                         exclude_varibs = c("dato", "inklusion", "complete", "komm", "test_upload"),
                         value_character = FALSE) { 
  guess_date <- as.logical(guess_date)
  value_character <- as.logical(value_character)
  stopifnot(is.character(name),
            length(name) == 1,
            is.logical(clean_part),
            length(clean_part) == 1,
            is.logical(guess_date),
            length(guess_date) == 1,
            is.logical(value_character),
            length(value_character) == 1)
  test_type <- match.arg(test_type)
  name_type <- match.arg(name_type)
  
  # ---
  # datalist <- dl2
  # name <- "facitf"
  # name_type <- "any"
  # test_type <- "none"
  # clean_part <- TRUE
  # test_part <- FALSE
  # guess_date = TRUE
  # dato_default = c("besoeg_dato", "screen_dato")
  # exclude_varibs <- c("dato", "inklusion", "complete", "komm")
  # value_character = FALSE

  # datalist <- dl2
  # name <- "gaf"
  # name_type <- "exact"
  # test_type <- "none"
  # clean_part <- FALSE
  # test_part <- FALSE
  # guess_date = TRUE
  # dato_default = c("besoeg_dato")
  # exclude_varibs <- c("none")
  # value_character = TRUE
  # 
  # show_variables(names=name)
  # ---
  
  txt <- name[1]
  if(name_type %in% c("begin", "exact")) txt <- paste0("^", txt)
  if(name_type %in% c("end", "exact")) txt <- paste0(txt, "$")
  
  lnms <- lapply(datalist, function(dd) getgrep(txt, names(dd)))
  do_visits <- names(which(sapply(lnms, length) > 0))
  d0a <- lapply(1:length(do_visits), function(i) { # i <- 1
    vis <- do_visits[i]
    dd <- datalist[[vis]]
    nms <- getgrep(txt, names(dd))
    if(length(exclude_varibs) & nchar(exclude_varibs[1]))
      nms2 <- nms[!grepl(paste0(exclude_varibs, collapse = "|"), nms)] else
        nms2 <- nms
    if(length(nms2) == 0) return(NULL)
    d0 <- dd[, .SD, .SDcols = nms2]
    d0
    
    ## Test and convert all data:
    if(test_type == "numeric") stopifnot(all(sapply(d0, is.numeric)))
    if(test_type == "integer") stopifnot(all(sapply(d0, is.integer)))
    if(test_type == "factor") stopifnot(all(sapply(d0, is.factor)))
    if(test_type == "character") stopifnot(all(sapply(d0, is.character)))
    measure_nms <- names(d0)
    if(value_character) d0[] <- lapply(d0, as.character)
    modes <- sapply(d0, mode)
    if(length(unique(modes)) > 1) {
      mat <- cbind(modes)
      print(mat)
      stop("Different modes found for: ", name, " visit: ", vis[i])
    }
    
    ## Find date-variable:
    # search for "dato" but exclude instances of "nydato"|"datoafbrudt" among nms:
    nms_dato <- if(guess_date & any(grepl("dato", nms))) 
      getgrep("dato", nms) else dato_default
    nms_dato <- nms_dato[nms_dato %in% names(dd)][1]
    stopifnot(length(nms_dato) == 1, nms_dato %in% names(dd))
    
    ## Add ID, visit and completion flags:
    d1 <- dd[, .(ID, date = get(nms_dato), date_var = nms_dato, 
                 c_visit = vis, variable = name)]
    if(length(nms_compl <- getgrep("complete", nms))) {
      d1 <- cbind(d1, dd[, .SD, .SDcols = nms_compl])
      setnames(d1, nms_compl, "assess_cmpl")
    }
    if("besgsafslutning_complete" %in% names(dd))
      d1 <- cbind(d1, dd[, .(bsg_cmpl = besgsafslutning_complete)])
    if("randomisering_og_medicinudlevering_complete" %in% names(dd))
      d1 <- cbind(d1, dd[, .(ranmed_cmpl = randomisering_og_medicinudlevering_complete)])
    if("besoeg_komplet" %in% names(dd))
      d1 <- cbind(d1, dd[, .(besoeg_kompl = besoeg_komplet)])
    if("repeat_instance" %in% names(dd))
      d1 <- cbind(d1, dd[, .(rep_inst = repeat_instance)])
    d0 <- cbind(d1, d0)
    
    ## Convert to long format:
    d0a <- melt(d0, measure.vars = measure_nms, 
                variable.factor = FALSE, variable.name = "orig", 
                value.factor = FALSE)
    d0a[, part := orig]
    d0a
  }) |> rbindlist(fill=TRUE)
  if(all(dim(d0a) == 0) | is.null(d0a)) return(d0a[])
  if(d0a[, is.character(value)]) setnames(d0a, "value", "c_value") else 
    if(d0a[, is.Date(value)]) setnames(d0a, "value", "d_value")
  # d0a[, .N, part]
  
  if(clean_part) {
    txt1 <- paste0( # text to remove from 'part':
      "^", name, "_|",  paste0("_dag", c(0, 4, 7, 14, 28), collapse = "|"), "|_6mdr")
    d0a[, part := gsub(txt1, "", part)]
    d0a[, part := gsub("hamd17_total", "total17", part)]
    # d0a[, part := gsub("tot6", "total6", part)]
    d0a[, part := gsub(paste0(name, "6_total"), "total6", part)]
    d0a[, part := gsub(paste0(name, "10_total"), "total10", part)]
    # d0a[, part := gsub("score", "", part)]
    if(test_part) {
      if(name == "mdi") stopifnot(d0a[, all(part %in% c(1:9, "total", paste0(rep(8:10, each=2), letters[1:2])))]) else 
        if(!name %in% c("bacs", "gaf")) 
          stopifnot(d0a[, all(part %in% c(1:34, "total", "total17", "total6", "total10", "scale"))])
    }
  }
  ## Return:
  d0a[]
}


## Check consistency of extracted DEXA data
##
## Description:
## Checks consistency of data extracted with 'get_dexadata'. In particular the function
## checks that (1) there is no more than one value for each combination of 
## ID x visit x variable x part; (2) all data has a non-missing date. 
## 
## The function generates a new variable 'use1' such that rows with 'use1 == 1' 
## can be used for analyses. To assign 'use1 == 1' data points are prioritized which has:
##  1. ranmed_cmpl == "Complete"
##  2. the latest date.
##
## Arguments: 
## - datalist    : A list of data.tables each containing a block of data corresponding
##   to each of the 11 the redcap event-names (one for each visit, one for events etc.)
## - names       : Vector of (partial) name of a variable to search for in DEXA data. 
## - print_empty : Print the names of blocks in which none of 'names' occur?
## - type        : Determines how 'names' is searched for. "begin" searches for 
##   variables that begin with 'names'; "end" searches for variables that end with 
##   "names"; 'exact' searches for variables exactly equal to "names" and; 'any' 
##   searches for any matches for "names" among variable names.
## 
## Return value:
## - Prints a variables names and where they occur in the DEXA data.
## 
check_multivalues <- function(data) {
  tmp <- copy(data)
  # tmp <- copy(d0a)
  
  ## Remove cases where all values are NA:
  tmp[, na_values := 1.0]
  if("value" %in% names(tmp)) tmp[!is.na(value), na_values := 0.0 ]
  if("c_value" %in% names(tmp)) tmp[!is.na(c_value), na_values := 0.0 ]
  if("d_value" %in% names(tmp)) tmp[!is.na(d_value), na_values := 0.0 ]
  tmp <- tmp[na_values == 0, -"na_values"]
  # tmp[, .N, na_values]
  # summary(tmp)
  
  ## Require no missing dates in data:
  stopifnot(tmp[is.na(date), .N == 0])
  
  ## Compute number of entries per ID x visit x variable x part:
  tmp[, nval := .N, .(ID, c_visit, variable, part)]
  # tmp[, .N, nval]
  
  ## Check that multi-values only occur for day 0:
  if(tmp[, "day0" %in% unique(c_visit)])
    stopifnot(tmp[nval > 1, all(c_visit == "day0")])
  
  ## For each combination of .(ID, c_visit, variable, part) we keep 1 value, and
  ##  we prioritize the value which has has:
  ##  1. ranmed_cmpl == "Complete"
  ##  2. the latest date.
  tmp[, use := 1.0 * (ranmed_cmpl == "Incomplete")]
  setorder(tmp, ID, c_visit, variable, part, use, -date)
  tmp[, use1 := 1:.N, .(ID, c_visit, variable, part)]
  tmp <- tmp[, -c("use", "nval")]
  
  ## Check that there are no duplicate combinations of 
  ##  .(ID, c_visit, variable, part) for 'use1 == 1':
  tmp1 <- tmp[use1 == 1, .(ID, c_visit, variable, part)]
  stopifnot(nrow(tmp1) == nrow(unique(tmp1)))
  
  ## Return:
  tmp[]
}
