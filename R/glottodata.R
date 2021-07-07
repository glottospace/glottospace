

#' Download glottolog data and convert it into a glot or geoglot object
#'
#' @param data One of \code{"glottodata_raw"}, \code{"glottodata_tidy"}, or
#'   \code{"glottodata_spatial"}. By default, the downloaded glottolog data is
#'   converted into a geoglot object (languages without coordinates are removed).
#'
#' @return Either a glot or geoglot object.
#' @export
#'
#' @examples
#' glottodata <- glottodata()
glottodata <- function(data = "glottodata_spatial"){
  strex::match_arg(data, c("glottodata_raw", "glottodata_spatial"), ignore_case = TRUE)
  if(data == "glottodata_raw"){
    data <- glottolog_download()
  }
  if(data == "glottodata_spatial"){
    d <- glottolog_download()
    data <- glottodata_spatial(glottologdata = d)
  }
  return(data)
}

glottolog_download <- function(){
  # FIXME: Now URL is fixed, which means that it doesn't update when newer version of glottolog becomes available.
  # TODO: try other URL if first one fails.
  # https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
  # from glottolog website or from zenodo.

  # https://github.com/cran/raster/blob/master/R/getData.R
  # https://rdrr.io/github/inbo/inborutils/src/R/download_zenodo.R
  base_url <- "https://cdstar.shh.mpg.de/bitstreams/EAEA0-D501-DBB8-65C4-0/"
  filename <- "glottolog_languoid.csv.zip"
  url <- paste0(base_url, filename)
  if(!base::file.exists(filename)){
  utils::download.file(url = url, destfile = filename)}
  data <- utils::read.csv(unz(filename, "languoid.csv"), header = TRUE)
}

glottodata_spatial <- function(glottologdata = NULL){
  if(is.null(glottologdata)){
    glottologdata <- glottolog_download(version = "glottolog")
    cat("No input data provided, glottolog data downloaded")
  }
  glottologlatlon <- glottologdata %>%
    dplyr::filter(!is.na(latitude)) %>%
    dplyr::filter(!is.na(longitude))

    data <- sf::st_as_sf(x = as.data.frame(glottologlatlon),
                     coords = c("longitude", "latitude"),
                     crs = 4326) #https://epsg.io/4326

}

# TODO: add CLDF
# References can be matched via lgcode.

glottolog_download_cldf(){
  # Built upon: https://github.com/SimonGreenhill/rcldf
base_url <-  "https://zenodo.org/api/records/4762034"
req <- curl::curl_fetch_memory(base_url)
content <- RJSONIO::fromJSON(rawToChar(req$content))
url <- content$files[[1]]$links[[1]]
filename = "glottolog_cldf.zip"
if(!base::file.exists(filename)){ download.file(url = url, destfile = filename) }

if(dir.exists("./cldf"))
utils::unzip(zipfile = filename, exdir = "./cldf")
list.dirs("./cldf")

# Create empty data
glottolog_cldf <- base::structure(list(tables = list()), class = "cldf")
glottolog_cldf$metadata <- jsonlite::fromJSON("cldf/cldf-metadata.json")
}

# else if(version == "glottolog_simple"){
#   # Partial glottolog: languages and dialects and coordinates.
#   # Information about family is not included in this dataset.
#   filename <- "languages_and_dialects_geo.csv"
#   url <- paste0(base_url, filename)
#   if(!base::file.exists(filename)){
#     download.file(url = url, destfile = filename)}
#   data <- utils::read.csv(filename)
# } else if(version == "glottolog_cldf"){
#   message('cldf not yet implemented')
#   # contact Simon Greenhill:
#   # https://github.com/SimonGreenhill/rcldf
#   # if (!require(curl)) {install.packages('curl')}
#   #   library(curl)
#   #   if (!require(RJSONIO)) {install.packages('RJSONIO')}
#   #   library(RJSONIO)
#   # if (!require(bib2df)) {install.packages('bib2df')}
#   # library(bib2df)
    base_url <-  "https://zenodo.org/api/records/4061165"
    req <- curl::curl_fetch_memory(base_url)
    content <- RJSONIO::fromJSON(rawToChar(req$content))
    url <- content$files[[1]]$links[[1]]
    filename = "glottolog_cldf.zip"
    if(!file.exists(filename)){
      download.file(url = url, destfile = filename)}
    unzip(filename)
  mdpath <- 'glottolog-glottolog-cldf-ac0d616/cldf/cldf-metadata.json'
  dir <- dirname(mdpath)
  o <- structure(list(tables = list()), class = "cldf")
  o$metadata <- jsonlite::fromJSON(mdpath)
  o$name <- dir
  o$type <- o$metadata$`dc:conformsTo`

  # load sources
  o$sources <- tryCatch({ read_bib(dir, o$metadata$`dc:source`) })

  get_tablename <- function(tbl) { tools::file_path_sans_ext(tbl) }

  get_table_schema <- function(schema) {
    spec <- list()
    for (i in 1:nrow(schema[[1]])) {
      label <- as.name(schema[[1]][i, "name"])
      spec[[label]] <- get_spec(schema[[1]][i, "datatype"])
    }
    do.call(readr::cols, spec)
  }

  for (i in 1:nrow(o$metadata$tables)) {
    filename <- file.path(dir, o$metadata$tables[i, "url"])
    table <- get_tablename(o$metadata$tables[i, "url"])
    cols <- get_table_schema(o$metadata$tables[i, "tableSchema"]$columns)

    o[["tables"]][[table]] <- vroom::vroom(
      filename, delim=",", col_names = TRUE, col_types = cols$cols, quote = '"'
    )

}

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

o
