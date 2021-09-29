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
#' glottogetbase()
glottogetbase <- function(glottodata = NULL){
   if(is.null(glottodata) ){
    glottodata <- get_glottobase()}
    else if(glottodata == "glottolog"){
    glottodata <- get_glottolog()
  } else if(glottodata == "glottobase"){
    glottodata <- get_glottobase()
  } else if (glottodata == "glottospace"){
    glottodata <- get_glottospace()
  } else {message("Unable to load requested glottodata")}

}

#' Load user-provided glottodata
#'
#' Load glottodadata/glottosubdata from a file, or create artificial dummy data.
#'
#' @param filename Path to glottodata file with extension (.xlsx .xls .gpkg .shp). If no filename is specified, an artificial dummy dataset will be created.
#' @param meta By default, meta sheets are not loaded. Use meta=TRUE if you want to include them.
#' @param simplify By default, if only one sheet is loaded, the data will be returned as a data.frame (instead of placing the data inside a list of length 1)
#' @param create In case 'filename' is not specified, artificial dummy data will be created in glottodata format (specify create = "glottosubdata" to create data in glottosubdata format)
#' @family <glottodata>
#' @return
#' @export
#' @seealso glottosave
#' @examples
#' glottoget()
#' glottoget(filename = "glottodata.xlsx")
#' glottoget(filename = "glottodata.gpkg")
glottoget <- function(filename = NULL, meta = FALSE, simplify = TRUE, create = "glottodata"){


  metasheets <- c("structure",  "metadata",   "references", "readme",     "lookup" )

  if(!is.null(filename)){
    if(tools::file_ext(filename) == ".xlsx" | tools::file_ext(filename) == ".xls"){
    sheetnames <- readxl::excel_sheets(filename)
  if(meta == TRUE){
    sheetnames <- sheetnames
  } else {
    sheetnames <- sheetnames[sheetnames %nin% metasheets]
  }
  glottodata <- base::lapply(X = sheetnames,
                         FUN = readxl::read_excel, path = filename)
  names(glottodata) <- sheetnames
    } else if(tools::file_ext(filename) == ".gpkg" | tools::file_ext(filename) == ".shp"){
      glottodata <- sf::st_read(dsn = filename)
    }

  } else {
    if(create == "glottodata"){glottodata <- createdummydata()
    } else if(create == "glottosubdata"){
      glottodata <- createdummysubdata()
    }
    sheetnames <- names(glottodata)
    if(meta == TRUE){
      sheetnames <- sheetnames
    } else {
      sheetnames <- sheetnames[sheetnames %nin% metasheets]
    }

    glottodata <- glottodata[sheetnames]

  }

  if(simplify == TRUE & length(glottodata) == 1 & any(class(glottodata) == "list") ){
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
#' glottobase <- get_glottobase()
get_glottobase <- function(){
  glottolog <- get_glottolog(data = "glottolog")
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
#' glottospace <- get_glottospace()
get_glottospace <- function(){
  glottologdata <- get_glottolog(data = "glottolog")
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
#' glottodata <- get_glottolog()
get_glottolog <- function(data = NULL){
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
  filename <- "glottolog_languoid.csv.zip"
  url <- paste0(base_url, filename)
  if(!base::file.exists(filename)){
    utils::download.file(url = url, destfile = filename)}
  data <- utils::read.csv(unz(filename, "languoid.csv"), header = TRUE)
}



glottolog_download_cldf <- function(destdir = tempdir()){
  base_url <-  "https://zenodo.org/api/records/4762034"
  req <- curl::curl_fetch_memory(base_url)
  content <- RJSONIO::fromJSON(rawToChar(req$content))
  # title <- gsub(".*:", "", content$metadata$title)
  # version <- content$metadata$version
  url <- content$files[[1]]$links[[1]]
  # filename <- base::basename(url)
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



