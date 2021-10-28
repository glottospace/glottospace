#' Get glottodata from local path or online global databases
#'
#' Load locally stored glottodata, download databases from online sources, or load built-in demo data
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
glottoget <- function(glottodata = NULL, meta = FALSE){
  if(is.null(glottodata)){
    glottodata <- glottoget_glottobase()
  } else if(glottodata == "glottobase"){
    glottodata <- glottoget_glottobase()
  } else if(glottodata == "glottolog"){
    glottodata <- glottoget_glottolog()
  } else if (glottodata == "glottospace"){
    glottodata <- glottoget_glottospace()
  } else if(glottodata == "demodata"){
    glottodata <- glottocreate_demodata(meta = meta)
  } else if(glottodata == "demosubdata"){
    glottodata <- glottocreate_demosubdata(meta = meta)
  } else if(tools::file_ext(glottodata) != ""){
    glottodata <- glottoget_path(filepath = glottodata, meta = meta)
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
#' Load glottodadata/glottosubdata from a file
#'
#' @param filepath Path to glottodata file with extension (.xlsx .xls .gpkg .shp). If no filepath is specified, an artificial demo dataset will be created.
#' @param meta By default, meta sheets are not loaded. Use meta=TRUE if you want to include them.
#' @param simplify By default, if only one sheet is loaded, the data will be returned as a data.frame (instead of placing the data inside a list of length 1)
#' @family <glottodata>
#' @return
#' @export
#' @seealso glottosave
#' @examples
#' glottoget_path()
#' glottoget_path(filepath = "glottodata.xlsx")
#' glottoget_path(filepath = "glottodata.gpkg")
glottoget_path <- function(filepath = NULL, meta = FALSE, simplify = TRUE){

  metasheets <- c("structure",  "metadata",   "references", "readme",     "lookup" )

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
  # TODO: build in fall back options, try one first, if that doesn't work, try next one.
  # Check whether file exists locally
  # Check local version
  # Check remote version
  # If newer version is available: ask user whether they want to download it
  # If that doesn't work, use built-in glottobase
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

glottoget_glottolog_v2 <- function(){
  if(glottolog_version_remote() > glottolog_version_local()){
  glottolog_download_cldf()
  }
}

glottolog_download <- function(){
  # FIXME: Now URL is fixed, which means that it doesn't update when newer version of glottolog becomes available.
  # TODO: try other URL if first one fails.
  # TODO: give message of which file version was downloaded, or loaded in case it was already available locally
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





#' Local directory with glottospace package files
#'
#' Loads path to local directory where glottospace files are stored and creates directory in a platform independent way in case it doesn't exist.
#'
#' @return
#' @export
#'
glottofiles_cachedir <- function(){
  cachedir <- base::normalizePath(rappdirs::user_data_dir("glottospace"), winslash = "\\", mustWork = FALSE)
  if(!base::dir.exists(cachedir)){
    base::dir.create(cachedir, recursive = TRUE)
  }
  cachedir
}

#' Create local path for a filename
#'
#' @param filename
#'
#' @return
#' @export
#'
glottofiles_makepath <- function(filename){
  filedir <- glottofiles_cachedir()
  filepath <- base::file.path(filedir, filename)
  normalizePath(filepath, winslash = "\\", mustWork = FALSE)
  # pkgfilecache::are_files_available(pkgfilecache::get_pkg_info("glottospace"), "something.gpkg")
}

#' Create local directory for a dirname
#'
#' @param dirname
#'
#' @return
#' @export
#'
glottofiles_makedir <- function(dirname){
  filedir <- glottofiles_cachedir()
  dirpath <- base::file.path(filedir, dirname)
  dirpath <- normalizePath(dirpath, winslash = "\\", mustWork = FALSE)
  if(!base::dir.exists(dirpath)){
    base::dir.create(dirpath, recursive = TRUE)
  }
  dirpath
}

glottolog_version_remote <- function(){
  base_url <-  "https://zenodo.org/api/records/4762034"
  req <- curl::curl_fetch_memory(base_url)
  content <- RJSONIO::fromJSON(rawToChar(req$content))
  # title <- gsub(".*:", "", content$metadata$title)
  as.numeric(gsub(pattern = "v", x = content$metadata$version, replacement = ""))
}

glottolog_version_local <- function(){
  dirs <- list.dirs(glottofiles_cachedir(), full.names = FALSE, recursive = FALSE)
  glottologdirs <- dirs[grepl(pattern = "glottolog-cldf-v", x = dirs)]
  # glottologdirs <- c("glottolog-cldf-v4.4", "glottolog-cldf-v4.5")
  versions <- gsub(pattern = "glottolog-cldf-v", x = glottologdirs, replacement = "")
  max(as.numeric(versions))
}

glottolog_download_cldf <- function(){
  base_url <-  "https://zenodo.org/api/records/4762034"
  req <- curl::curl_fetch_memory(base_url)
  content <- RJSONIO::fromJSON(rawToChar(req$content))
  url <- content$files[[1]]$links[[1]]
  filename <- base::basename(url)
  filepath <- glottofiles_makepath(filename)
  exdir <- glottofiles_makedir(tools::file_path_sans_ext(filename))
  utils::download.file(url = url, destfile = filepath )
  utils::unzip(zipfile = filepath, exdir = exdir)
  message(paste(gsub(".*:", "", content$metadata$title), "downloaded"))
  # FIXME: regex *-metadata.json
  cldf_metadata <- base::list.files(exdir, pattern = "cldf-metadata.json", recursive = TRUE)
  base::dirname(normalizePath(file.path(exdir, cldf_metadata)))
}
