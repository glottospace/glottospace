#' Get glottodata from local path or online global databases
#'
#' Load locally stored glottodata, download databases from online sources, or load built-in dummy data
#'
#' @param glottodata options are:
#' \itemize{
#' \item A filepath to locally stored glottodata with file extension (.xlsx .xls .gpkg
#' .shp). See also: options meta and simplify.
#' \item "glottobase" - Default option, an enhanced version of \href{https://glottolog.org/}{glottolog}. See
#' \link{glottologbooster} for details.
#' \item "glottolog" - This is the raw data downloaded from \href{https://glottolog.org/}{glottolog}
#' \item "glottospace" - A simple dataset with glottocodes and a geometry column. This
#' is a subset of all languages in \href{https://glottolog.org/}{glottolog} with
#' spatial coordinates.
#' \item "demodata" - Built-in artificial glottodata (included for demonstration and testing)
#' \item "demosubdata" - Built-in artificial glottosubdata (included for demonstration and testing)
#' }
#' @param meta In case 'glottodata' is a path to locally stored data (or demodata/demosubdata): by default, meta sheets are not loaded. Use meta=TRUE if you want to include them.
#' @param simple In case 'glottodata' is a path to locally stored data  (or demodata/demosubdata): by default, if only one sheet is loaded, the data will be returned as a data.frame (instead of placing the data inside a list of length 1)
#'
#' @family <glottodata>
#' @return
#' @export
#' @examples
#' glottoget()
glottoget <- function(glottodata = NULL, meta = FALSE, simple = TRUE){
  if(is.null(glottodata)){
    glottodata <- glottoget_glottobase()
  } else if(glottodata == "glottobase"){
    glottodata <- glottoget_glottobase()
  } else if(glottodata == "glottolog"){
    glottodata <- glottoget_glottolog()
  } else if (glottodata == "glottospace"){
    glottodata <- glottoget_glottospace()
  }  else if(glottodata == "demodata"){
    glottodata <- glottoget_path(meta = meta, simple = simple, create = "glottodata")
  } else if(glottodata == "demosubdata"){
    glottodata <- glottoget_path(meta = meta, simple = simple, create = "glottosubdata")
  } else if(tools::file_ext(glottodata) != ""){
    glottodata <- glottoget_path(filepath = glottodata, meta = meta, simple = simple)
  } else {message("Unable to load requested glottodata")}
return(glottodata)
}

#' Get glottodata from online global databases
#'
#' Get glottodata from online sources
#'
#' @param glottodata options are:
#' "glottobase" - default option, an enhanced version of href{https://glottolog.org/}{glottolog}. See \link{glottologbooster} for details.
#' "glottolog" - this is the raw data downloaded from \href{https://glottolog.org/}{glottolog}
#' "glottospace" - a simple dataset with glottocodes and a geometry column. This is a subset of all languages in href{https://glottolog.org/}{glottolog} with spatial coordinates.
#'
#' @family <glottodata>
#' @return
#' @export
#' @examples
#' glottoget_remote()
glottoget_remote <- function(glottodata = NULL){
  if(is.null(glottodata) ){
    glottodata <- glottoget_glottobase()}
  else if(glottodata == "glottolog"){
    glottodata <- glottoget_glottolog()
  } else if(glottodata == "glottobase"){
    glottodata <- glottoget_glottobase()
  } else if (glottodata == "glottospace"){
    glottodata <- glottoget_glottospace()
  } else {message("Unable to load requested glottodata")}
}


#' Load user-provided glottodata
#'
#' Load glottodadata/glottosubdata from a file, or create artificial dummy data.
#'
#' @param filepath Path to glottodata file with extension (.xlsx .xls .gpkg .shp). If no filepath is specified, an artificial dummy dataset will be created.
#' @param meta By default, meta sheets are not loaded. Use meta=TRUE if you want to include them.
#' @param simple By default, if only one sheet is loaded, the data will be returned as a data.frame (instead of placing the data inside a list of length 1)
#' @param create In case 'filepath' is not specified, artificial dummy data will be created in glottodata format (specify create = "glottosubdata" to create data in glottosubdata format)
#' @family <glottodata>
#' @return
#' @export
#' @seealso glottosave
#' @examples
#' glottoget_path()
#' glottoget_path(filepath = "glottodata.xlsx")
#' glottoget_path(filepath = "glottodata.gpkg")
glottoget_path <- function(filepath = NULL, meta = FALSE, simple = TRUE, create = NULL){
  if(is.null(create) ){create <- "glottodata"}


  metasheets <- c("structure",  "metadata",   "references", "readme",     "lookup" )

  if(!is.null(filepath)){
    if(tools::file_ext(filepath) == ".xlsx" | tools::file_ext(filepath) == ".xls"){
    sheetnames <- readxl::excel_sheets(filepath)
  if(meta == TRUE){
    sheetnames <- sheetnames
  } else {
    sheetnames <- sheetnames[sheetnames %nin% metasheets]
  }
  glottodata <- base::lapply(X = sheetnames,
                         FUN = readxl::read_excel, path = filepath)
  names(glottodata) <- sheetnames
    } else if(tools::file_ext(filepath) == ".gpkg" | tools::file_ext(filepath) == ".shp"){
      glottodata <- sf::st_read(dsn = filepath)
    }

  } else {
    if(create == "glottodata"){glottodata <- glottocreate_dummydata()
    } else if(create == "glottosubdata"){
      glottodata <- glottocreate_dummysubdata()
    }
    sheetnames <- names(glottodata)
    if(meta == TRUE){
      sheetnames <- sheetnames
    } else {
      sheetnames <- sheetnames[sheetnames %nin% metasheets]
    }

    glottodata <- glottodata[sheetnames]

  }

  if(simple == TRUE & length(glottodata) == 1 & any(class(glottodata) == "list") ){
    glottodata <- glottodata[[1]]
  }
  return(glottodata)
}


#' Get glottobase reference data
#'
#' Downloads most recent glottolog data and transforms it. This 'glottobase' is used as reference dataset in several functions.
#'
#' @return
#' @export
#'
#' @examples
#' glottobase <- glottoget_glottobase()
glottoget_glottobase <- function(){
  glottolog <- glottoget_glottolog(data = "glottolog")
  glottobase <- glottologbooster(glottologdata = glottolog)
  glottobase
}

#' Get glottospace reference data
#'
#' Get most recent glottolog data and turn it into the most elemental geoglot object (i.e. glottocodes + geometry column). This 'glottospace' is used as reference dataset in several functions.
#'
#' @return
#' @export
#' @seealso glottodata_addcoords
#'
#' @examples
#' glottospace <- glottoget_glottospace()
glottoget_glottospace <- function(){
  glottologdata <- glottoget_glottolog(data = "glottolog")
  glottologdata <- glottologdata %>% dplyr::rename("glottocode" = "id")
  glottospace <- glot2geoglot(glottologdata)
  glottospace <- glottospace[,c("glottocode")]
}



#' Download glottolog data
#'
#' @param data One of \code{"glottolog"} or
#'   \code{"glottolog_cldf"}.
#'
#' @return Either a glot or geoglot object.
#' @export
#'
#' @examples
#' glottodata <- glottoget_glottolog()
glottoget_glottolog <- function(data = NULL){
  if(is.null(data) ){data <- "glottolog"}
  if(data == "glottolog"){
    glottodata <- glottolog_download()
  } else if(data == "glottolog_cldf"){
    cldfpath <- glottolog_download_cldf()
    glottodata <- read_cldf(mdpath = cldfpath)
  } else {
    stop("Could not get glottolog data")
  }
  return(glottodata)
}

glottolog_download <- function(){
  # FIXME: Now URL is fixed, which means that it doesn't update when newer version of glottolog becomes available.
  # TODO: try other URL if first one fails.
  # https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
  # from glottolog website or from zenodo.

  # https://github.com/cran/raster/blob/master/R/getData.R
  # https://rdrr.io/github/inbo/inborutils/src/R/download_zenodo.R
  base_url <- "https://cdstar.shh.mpg.de/bitstreams/EAEA0-E62D-ED67-FD05-0/"
  filepath <- "glottolog_languoid.csv.zip"
  url <- paste0(base_url, filepath)
  if(!base::file.exists(filepath)){
    utils::download.file(url = url, destfile = filepath)}
  data <- utils::read.csv(unz(filepath, "languoid.csv"), header = TRUE, encoding = "UTF-8")
}



glottolog_download_cldf <- function(destdir = tempdir()){
  base_url <-  "https://zenodo.org/api/records/4762034"
  req <- curl::curl_fetch_memory(base_url)
  content <- RJSONIO::fromJSON(rawToChar(req$content))
  # title <- gsub(".*:", "", content$metadata$title)
  # version <- content$metadata$version
  url <- content$files[[1]]$links[[1]]
  # filepath <- base::basename(url)
  destdir <- paste0(normalizePath(destdir, winslash = "/", mustWork = FALSE), "/cldf")
  tmpfile <- tempfile()
  # tmpfile <- tempfile(tmpdir = destdir)
  utils::download.file(url = url, destfile = tmpfile )
  utils::unzip(zipfile = tmpfile, exdir = destdir)
  # FIXME: regex *-metadata.json
  cldf_md <- base::list.files(destdir, pattern = "cldf-metadata.json", recursive = TRUE)
  cldfpath <- paste(destdir,
                    stringr::str_remove(cldf_md, "cldf-metadata.json"), sep = "/" )
  return(cldfpath)
}



