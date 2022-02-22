#' Get glottodata from local path or online global databases
#'
#' Load locally stored glottodata, download databases from online sources, or load built-in demo data
#'
#' @param glottodata options are:
#' \itemize{
#' \item A filepath to locally stored glottodata with file extension (.xlsx .xls .gpkg
#' .shp). See also: options meta and simplify.
#' \item "glottobase" - Default option, an spatially enhanced version of \href{https://glottolog.org/}{glottolog}. See
#' \link{glottobooster} for details.
#' \item "wals" - This is a spatial and enhanced version of \href{https://wals.info/}{WALS}.
#' \item "glottolog" - This is a restructured (non-spatial) version of \href{https://glottolog.org/}{glottolog}.
#' \item "glottospace" - A simple dataset with glottocodes and a geometry column. This
#' is a subset of all languages in \href{https://glottolog.org/}{glottolog} with
#' spatial coordinates.
#' \item "demodata" - Built-in artificial glottodata (included for demonstration and testing)
#' \item "demosubdata" - Built-in artificial glottosubdata (included for demonstration and testing)
#' }
#' @param meta In case 'glottodata' is a path to locally stored data (or demodata/demosubdata): by default, meta sheets are not loaded. Use meta=TRUE if you want to include them.
#' @param download By default internally stored versions of global databases are used. Specify download = TRUE in case you want to download the latest version from a remote server.
#' @param dirpath Optional, if you want to store a global CLDF dataset in a specific directory, or load it from a specific directory.
#' @param url Zenodo url, something like this: "https://zenodo.org/api/records/1234567"
#'
#' @family <glottodata>
#' @return A glottodata or glottosubdata object (a data.frame or list, depending on which glottodata is requested)
#' @export
#' @examples
#' \donttest{
#' glottoget("glottolog")
#' }
glottoget <- function(glottodata = NULL, meta = FALSE, download = FALSE, dirpath = NULL, url = NULL){
  if(is.null(glottodata)){
    glottodata <- glottoget_glottobase(download = download, dirpath = dirpath)
  } else if(glottodata == "glottobase"){
    glottodata <- glottoget_glottobase(download = download, dirpath = dirpath)
  } else if(glottodata == "glottolog"){
    glottodata <- glottoget_glottolog(download = download, dirpath = dirpath)
  } else if (glottodata == "glottospace"){
    glottodata <- glottoget_glottospace(download = download, dirpath = dirpath)
  } else if(glottodata == "demodata"){
    glottodata <- glottocreate_demodata(meta = meta)
  } else if(glottodata == "demosubdata"){
    glottodata <- glottocreate_demosubdata(meta = meta)
  } else if(glottodata == "wals"){
    glottodata <- glottoget_wals(download = download)
  } else if(!is.null(url)){
    glottoget_zenodo(url = url, dirpath = dirpath)
  } else if(tools::file_ext(glottodata) != ""){
    glottodata <- glottoget_path(filepath = glottodata)
  } else {message("Unable to load requested glottodata")}
return(glottodata)
}

#' Load glotto(sub)data from file
#'
#' Load glottodata/glottosubdata from a file
#'
#' @param filepath Path to glottodata file with extension (.xlsx .xls .gpkg .shp). If no filepath is specified, an artificial demo dataset will be created.
#' @param simplify By default, if only one sheet is loaded, the data will be returned as a data.frame (instead of placing the data inside a list of length 1)
#' @family <glottodata>
#'
#' @noRd
#' @seealso glottosave
#' @examples
#' \dontrun{
#' glottoget_path()
#' glottoget_path(filepath = "glottodata.xlsx")
#' glottoget_path(filepath = "glottodata.gpkg")
#' }
glottoget_path <- function(filepath = NULL, simplify = TRUE){

  # metasheets <- names(glottocreate_metatables())

  if(tools::file_ext(filepath) == "xlsx" | tools::file_ext(filepath) == "xls"){
    sheetnames <- readxl::excel_sheets(filepath)
  # if(meta == TRUE){
  #   sheetnames <- sheetnames
  # } else {
  #   sheetnames <- sheetnames[sheetnames %nin% metasheets]
  # }
  glottodata <- base::lapply(X = sheetnames,
                         FUN = readxl::read_excel, path = filepath)
  names(glottodata) <- sheetnames

    } else if(tools::file_ext(filepath) == "csv"){
      glottodata <- utils::read.csv(filepath, header = TRUE, encoding = "UTF-8")
    } else if(tools::file_ext(filepath) == "gpkg" | tools::file_ext(filepath) == "shp"){
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
#' @param ... Arguments to glottobooster
#'
#' @param download By default internally stored versions of global databases are used. Specify download = TRUE in case you want to download the latest version from a remote server.
#'
#' @noRd
#'
#' @examples
#' glottobase <- glottoget_glottobase()
glottoget_glottobase <- function(download = NULL, dirpath = NULL, ...){
  glottolog <- glottoget_glottolog(download = download, dirpath = dirpath)
  glottobase <- glottobooster(glottologdata = glottolog, ...)
  glottobase
}

#' Get glottospace reference data
#'
#' Get most recent glottolog data and turn it into the most elemental geoglot object (i.e. glottocodes + geometry column). This 'glottospace' is used as reference dataset in several functions.
#'
#'
#' @param download By default internally stored versions of global databases are used. Specify download = TRUE in case you want to download the latest version from a remote server.
#'
#' @noRd
#' @seealso glottospace_addcoords
#'
#'
#' @examples
#' glottospace <- glottoget_glottospace()
glottoget_glottospace <- function(download = NULL, dirpath = NULL){
  glottologdata <- glottoget_glottolog(download = download, dirpath = dirpath)
  glottologdata <- glottologdata %>% dplyr::rename("glottocode" = "id")
  glottospace <- glottospace_coords2sf(glottologdata)
  glottospace <- glottospace[,c("glottocode")]
  glottospace
}





#' Get glottolog data
#'
#' This function checks whether most recent version of glottolog is locally available. If local version is outdated, the newest version will be downloaded.
#'
#'
#' @noRd
#'
#' @examples
#' glottoget_glottolog()
glottoget_glottolog <- function(download = NULL, dirpath = NULL){
  if(is.null(download)){
    download <- FALSE
    }
  if(download == FALSE & is.null(dirpath) ){
    out <- glottospace::glottolog
  } else if(download == FALSE & !is.null(dirpath)){
    out <- glottolog_loadlocal(dirpath = dirpath)
  } else if(download == TRUE){
    out <- glottolog_download(dirpath = dirpath)
  }
return(out)
}

#' Download glottolog data
#'
#' @noRd
#'
glottolog_download <- function(dirpath = NULL){
  invisible(readline(prompt="Are you sure you want to download Glottolog data? \n Press [enter] to continue"))
  dirpath <- glottoget_zenodo(name = "glottolog", dirpath = dirpath)
  glottolog_loadlocal(dirpath = dirpath)
}

#' Check what's the most recent version of glottolog
#'
#' @noRd
glottolog_version_remote <- function(){
  base_url <-  "https://zenodo.org/api/records/3260727"
  message("Checking what's the most recent version of glottolog ... this might take a while")
  req <- curl::curl_fetch_memory(base_url)
  content <- RJSONIO::fromJSON(rawToChar(req$content))
  # title <- gsub(".*:", "", content$metadata$title)
  as.numeric(gsub(pattern = "v", x = content$metadata$version, replacement = ""))
}

#' Check which version of glottolog is available on your computer
#'
#'
#' @noRd
glottolog_version_localdir <- function(){
  dirs <- list.dirs(glottofiles_cachedir(), full.names = FALSE, recursive = FALSE)
  if(purrr::is_empty(dirs)){
    return(0)
  } else{
    glottologdirs <- dirs[grepl(pattern = "glottolog-cldf-v", x = dirs)]
    if(purrr::is_empty(glottologdirs)){
      return(0)
    } else{
    versions <- gsub(pattern = "glottolog-cldf-v", x = glottologdirs, replacement = "")
    return(max(as.numeric(versions)))
    }
  }

}

#' Check which version of glottolog is available on your computer
#'
#'
#' @noRd
glottolog_version_local <- function(){
  files <- base::list.files(glottofiles_cachedir(), full.names = FALSE, recursive = FALSE)
  if(purrr::is_empty(files)){
    return(0)
  } else{
    glottologfiles <- files[base::grepl(pattern = "glottolog-cldf-v", x = files)]
    glottologzips <- glottologfiles[grepl(pattern = ".zip", x = glottologfiles)]
    if(purrr::is_empty(glottologzips)){
      return(0)
    } else{
      versionzips <- base::gsub(pattern = "glottolog-cldf-v", x = glottologzips, replacement = "")
      versions <- tools::file_path_sans_ext(versionzips)
      return(max(as.numeric(versions)))
    }
  }

}

#' Reset last modified date of glottolog
#'
#'
#' @noRd
glottolog_version_localdatereset <- function(){
  v <- glottolog_version_local()
  newestpath <- glottofiles_makepath(paste0("glottolog-cldf-v", version, ".zip"))
  file.info(newestpath)$mtime
  Sys.setFileTime(newestpath, Sys.time())
}

#' Check how long ago glottolog was last downloaded
#'
#' @return Number of days passed since glottolog data was downloaded for the last time
#' @noRd
glottolog_date_local <- function(){
  v <- glottolog_version_local()
 if(v != 0){
   newestpath <- glottofiles_makepath(paste0("glottolog-cldf-v", v, ".zip"))
   glottolog_time <- file.info(newestpath)$mtime
   daysago <- lubridate::as.duration(lubridate::interval(Sys.time(), glottolog_time)) %/% lubridate::as.duration(lubridate::days(1))
   return(daysago)
 } else{
   return(-999999)
 }
}

#' Load locally stored glottolog data
#'
#' @param dirpath Path to directory where glottolog cldf data is stored
#'
#' @importFrom rlang .data
#' @noRd
glottolog_loadlocal <- function(dirpath){
  if(!dir.exists(dirpath)){stop("Directory not found.")}
  cldf_metadata <- base::list.files(dirpath, pattern = "cldf-metadata.json", recursive = TRUE)
  mdpath <- normalizePath(file.path(dirpath, cldf_metadata))
  mddir <- normalizePath(base::dirname(mdpath))

  # Load languages file
  languoids <- normalizePath(file.path(mddir, "languages.csv"))
  languoids <- utils::read.csv(languoids, header = TRUE, encoding = "UTF-8")
  colnames(languoids) <- base::tolower(colnames(languoids))
  colnames(languoids)[which(colnames(languoids) == "id")] <- "lang_id"


  # Load values file
  values <- normalizePath(file.path(mddir, "values.csv"))
  values <- utils::read.csv(values, header = TRUE, encoding = "UTF-8")
  colnames(values) <- base::tolower(colnames(values))
  colnames(values)[colnames(values) == "language_id"] <- "lang_id"
  values <- tidyr::pivot_wider(data = values, names_from = "parameter_id", values_from = "value")

  levels <- values[!is.na(values$level), c("lang_id", "level")]
  category <- values[!is.na(values$category), c("lang_id", "category")]
  category$bookkeeping <- base::apply(category[,"category"], 1, function(x){ifelse(tolower(x) == "bookkeeping", TRUE, FALSE)})
  classification <- values[!is.na(values$classification), c("lang_id", "classification")]
  classification$parent_id <- base::apply(classification[,"classification"], 1, function(x){sub(".*/", "", x)})

  glottologdata <- languoids %>% dplyr::left_join(levels, by = "lang_id") %>%
    dplyr::left_join(category, by = "lang_id") %>%
    dplyr::left_join(classification, by = "lang_id") %>%
    dplyr::arrange(.data$lang_id)

  colnames(glottologdata)[which(colnames(glottologdata) == "lang_id")] <- "id"
  glottologdata <- glottologdata %>% dplyr::select(-.data$glottocode, -.data$language_id)
  invisible(glottologdata)
}


#' Download database from Zenodo
#'
#' @param name Name of a dataset, either wals or glottolog
#' @param dirpath Path to directory where files should be stored
#' @param url Zenodo url, something like this: "https://zenodo.org/api/records/1234567"
#'
#' @noRd
glottoget_zenodo <- function(name = NULL, url = NULL, dirpath = NULL){

  if(is.null(name) & !is.null(url)){
    base_url <- url
  } else if(tolower(name) == "glottolog"){
    # Newest version is always uploaded here!
    base_url <- "https://zenodo.org/api/records/3260727"
  } else if(tolower(name) == "wals"){
    # Newest version is always uploaded here!
    base_url <- "https://zenodo.org/api/records/3606197"
  } else if(!is.null(name) ){
    stop("Unable to download data from Zenodo. Unrecognized name argument. ")
  }

  if(is.null(dirpath)){dirpath <- tempdir()}

  req <- curl::curl_fetch_memory(base_url)
  content <- RJSONIO::fromJSON(rawToChar(req$content))
  url <- content$files[[1]]$links[[1]]

  filepath <- tempfile()
  utils::download.file(file.path(url), destfile = filepath)
  utils::unzip(zipfile = filepath, exdir = dirpath)

  if(tolower(name) == "glottolog"){
    message(paste0("Glottolog data downloaded (glottolog ", content$metadata$version,"). This is the most recent version available from ", base_url) )
  } else if(tolower(name) == "wals"){
    message(paste0("WALS data downloaded (wals-", content$metadata$version,"). This is the most recent version available from ", base_url) )
  }
invisible(dirpath)

}

