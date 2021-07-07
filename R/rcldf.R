
# Functions obtained from https://github.com/SimonGreenhill/rcldf

#' Reads a Cross-Linguistic Data Format dataset into an object.
#'
#' @param mdpath the path to the directory or metadata.json file.
#' @return A `cldf` object
#' @export
#' @examples
#' cldfobj <- cldf(system.file("extdata/huon", "cldf-metadata.json", package = "rcldf"))
read_cldf <- function(mdpath) {
  mdpath <- resolve_path(mdpath)
  dir <- dirname(mdpath)
  o <- structure(list(tables = list()), class = "cldf")
  o$metadata <- jsonlite::fromJSON(mdpath)
  o$name <- dir
  o$type <- o$metadata$`dc:conformsTo`

  # load sources
  o$sources <- tryCatch({ read_bib(dir, o$metadata$`dc:source`) })

  for (i in 1:nrow(o$metadata$tables)) {
    filename <- file.path(dir, o$metadata$tables[i, "url"])
    table <- get_tablename(o$metadata$tables[i, "url"])
    cols <- get_table_schema(o$metadata$tables[i, "tableSchema"]$columns)

    o[["tables"]][[table]] <- vroom::vroom(
      filename, delim=",", col_names = TRUE, col_types = cols$cols, quote = '"'
    )
  }
  o
}

#' Helper function to resolve the path (e.g. directory or md.json file)
#' Developed by Simon Greenhill: https://github.com/SimonGreenhill/rcldf/blob/master/R/resolve_path.R
#' @param path the path to resolve
#' @export
#' @return A string containing the path to the metadata.json file
resolve_path <- function(path) {
  path <- base::normalizePath(path, mustWork = FALSE)
  if (file.exists(path) & endsWith(path, ".json")) {
    # given a metadata.json file
    mdfile <- path
  } else if (dir.exists(path)) {
    # given a dirname, try find the metadata file.
    mdfile <- list.files(path, "*.json", full.names = TRUE)
  } else if (!file.exists(path)) {
    stop(sprintf("Path %s does not exist", path))
  } else {
    stop(
      "Need either a metadata.json file or a directory with metadata.json"
    )
  }
  mdfile
}



#' Reads a BibTeX file into a dataframe
#'
#' @param dir the directory the BibTeX file is in.
#' @param bib the name of the BibTeX file (default="sources.bib")
#' @return A tibble dataframe
read_bib <- function(dir, bib="sources.bib"){
  if (is.null(bib)) return(NA)
  bib <- file.path(dir, bib)
  if (!file.exists(bib)) return(NA)
  bib2df::bib2df(bib)
}

#' Convert a CLDF URL tablename to a short tablename
#'
#' @param tbl the tablename.
#' @return A string
#' @export
#' @examples
#' get_tablename("languages.csv")
get_tablename <- function(tbl) { tools::file_path_sans_ext(tbl) }

#' Extracts the table schema from the metadata schema
#'
#' @param schema the metadata schema.
#' @return A column schema
get_table_schema <- function(schema) {
  spec <- list()
  for (i in 1:nrow(schema[[1]])) {
    label <- as.name(schema[[1]][i, "name"])
    spec[[label]] <- get_spec(schema[[1]][i, "datatype"])
  }
  do.call(readr::cols, spec)
}

#' Returns a dataset specification for `readr` or `vroom` from the metadata file
#'
#' @param dt a column schema spec
#' @return a `readr` column spec
get_spec <- function(dt) {
  dt <- unlist(dt)
  # if there's no col type specified, then the CLDF default is string.
  if (is.null(dt) | all(is.na(dt))) {
    return(readr::col_character())
  }
  # collapse a complex datatype to its base datatype.
  if ('base' %in% names(dt)) dt <- dt['base']

  if (dt == "string") {
    return(readr::col_character())
  } else if (dt == "decimal") {
    return(readr::col_double())
  } else if (dt == "integer") {
    return(readr::col_integer())
  } else if (dt == "float") {
    return(readr::col_double())
  } else if (dt == "boolean") {
    return(readr::col_logical())
  } else {
    warning(paste("Unable to identify coltype", dt))
    return(readr::col_guess())
  }
}

get_spec <- compiler::cmpfun(get_spec)
