#' Get glottodata from local path or online global databases
#'
#' Load locally stored glottodata, download databases from online sources, or load built-in demo data
#'
#' @param glottodata options are:
#' \itemize{
#' \item A filepath to locally stored glottodata with file extension (.xlsx .xls .gpkg
#' .shp). See also: options meta and simplify.
#' \item "glottobase" - Default option, an spatially enhanced version of \href{https://glottolog.org/}{glottolog}. See
#' \link{glottologbooster} for details.
#' \item "wals" - This is a spatial and enhanced version of \href{https://wals.info/}{WALS}.
#' \item "glottolog" - This is a restructured (non-spatial) version of \href{https://glottolog.org/}{glottolog}.
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
#' glottoget("glottolog")
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
  } else if(glottodata == "wals"){
    glottodata <- glottoget_wals()
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
#' @keywords internal
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


#' Load glotto(sub)data from file
#'
#' Load glottodata/glottosubdata from a file
#'
#' @param filepath Path to glottodata file with extension (.xlsx .xls .gpkg .shp). If no filepath is specified, an artificial demo dataset will be created.
#' @param meta By default, meta sheets are not loaded. Use meta=TRUE if you want to include them.
#' @param simplify By default, if only one sheet is loaded, the data will be returned as a data.frame (instead of placing the data inside a list of length 1)
#' @family <glottodata>
#' @return
#' @export
#' @keywords internal
#' @seealso glottosave
#' @examples
#' glottoget_path()
#' glottoget_path(filepath = "glottodata.xlsx")
#' glottoget_path(filepath = "glottodata.gpkg")
glottoget_path <- function(filepath = NULL, meta = FALSE, simplify = TRUE){

  metasheets <- c("structure",  "metadata",   "references", "readme",     "lookup" )

  if(tools::file_ext(filepath) == "xlsx" | tools::file_ext(filepath) == "xls"){
    sheetnames <- readxl::excel_sheets(filepath)
  if(meta == TRUE){
    sheetnames <- sheetnames
  } else {
    sheetnames <- sheetnames[sheetnames %nin% metasheets]
  }
  glottodata <- base::lapply(X = sheetnames,
                         FUN = readxl::read_excel, path = filepath)
  names(glottodata) <- sheetnames
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
#' @param ... Arguments to glottologbooster
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
#' glottobase <- glottoget_glottobase()
glottoget_glottobase <- function(...){
  glottolog <- glottoget_glottolog()
  glottobase <- glottologbooster(glottologdata = glottolog, ...)
  glottobase
}

#' Get glottospace reference data
#'
#' Get most recent glottolog data and turn it into the most elemental geoglot object (i.e. glottocodes + geometry column). This 'glottospace' is used as reference dataset in several functions.
#'
#' @return
#' @export
#' @seealso glottodata_addcoords
#' @keywords internal
#'
#' @examples
#' glottospace <- glottoget_glottospace()
glottoget_glottospace <- function(){
  glottologdata <- glottoget_glottolog()
  glottologdata <- glottologdata %>% dplyr::rename("glottocode" = "id")
  glottospace <- glot2geoglot(glottologdata)
  glottospace <- glottospace[,c("glottocode")]
}





#' Get glottolog data
#'
#' This function checks whether most recent version of glottolog is locally available. If local version is outdated, the newest version will be downloaded.
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
#' glottoget_glottolog()
glottoget_glottolog <- function(days = NULL){
  if(is.null(days)){days <- 30}
  if(curl::has_internet() & glottolog_date_local() < (-days) ){
    message(paste("Your local version of glottolog was downloaded more than ", days, " days ago."))
    vremote <- glottolog_version_remote()
    vlocal <- glottolog_version_local()
    if(vremote == vlocal){
      out <- glottolog_loadlocal()
      message(paste("Glottolog is up-to-date. Version", vlocal, " loaded."))
    } else if(vremote > vlocal){
      out <- glottolog_download()
    }
  } else { # Try to load local data, or else load built-in data.
    out <- try(
      expr = glottolog_loadlocal(),
      silent = TRUE
    )
    if(class(out) == "try-error"){
      data("glottolog")
      out <- glottolog
    }

  }
return(out)
}



#' Download glottolog data
#'
#' This function tries to download glottolog data from zenodo in cldf format. If that fails, it tries to download from glottolog.org
#' @noRd
#' @return
#' @export
#' @keywords internal
#'
glottolog_download <- function(){
  out <- try(
    expr = glottolog_download_cldf(),
    silent = TRUE
  )
  if(class(out) == "try-error"){
    out <- try(
      expr = glottolog_download_webpage(),
      silent = TRUE
    )
  }
  if(class(out) != "try-error"){
    return(out)
  } else {
    message("Unable to download glottolog data.")
  }
}






#' Local directory with glottospace package files
#'
#' Loads path to local directory where glottospace files are stored and creates directory in a platform independent way in case it doesn't exist.
#'
#' @return
#' @export
#' @noRd
glottofiles_cachedir <- function(){
  cachedir <- base::normalizePath(rappdirs::user_data_dir("glottospace"), winslash = "\\", mustWork = FALSE)
  if(!base::dir.exists(cachedir)){
    base::dir.create(cachedir, recursive = TRUE)
  }
  cachedir
}

#' Create local path for a filename
#'
#' @param filename filename
#' @noRd
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
#' @param dirname dirname
#' @noRd
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

#' Check what's the most recent version of glottolog
#'
#' @return
#' @export
#' @keywords internal
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
#' @return
#' @export
#' @keywords internal
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
#' @return
#' @export
#' @keywords internal
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

#' Check how long ago glottolog was downloaded
#'
#' @return Number of days passed since glottolog data was downloaded for the last time
#' @export
#' @keywords internal
glottolog_date_local <- function(){
  v <- glottolog_version_local()
 if(v != 0){
   newestdir <- glottofiles_makepath(paste0("glottolog-cldf-v", v))
   glottolog_time <- file.info(newestdir)$ctime
   daysago <- lubridate::as.duration(lubridate::interval(Sys.time(), glottolog_time)) %/% lubridate::as.duration(lubridate::days(1))
   return(daysago)
 } else{
   return(-999999)
 }
}

#' Download glottolog data from glottolog website
#'
#' @return
#' @export
#' @keywords internal
glottolog_download_webpage <- function(){
  base_url <- "https://cdstar.shh.mpg.de/bitstreams/EAEA0-E62D-ED67-FD05-0/" # Newest version is always uploaded here!
  filename <- "glottolog_languoid.csv.zip"
  url <- paste0(base_url, filename)
  filepath <- glottofiles_makepath(filename)

  utils::download.file(url = url, destfile = filepath) # always downloads newest version (overwrites previous one)
  exdir <- glottofiles_makedir(tools::file_path_sans_ext(tools::file_path_sans_ext(filename)))
  utils::unzip(zipfile = filepath, exdir = exdir)
  glottologdata <- utils::read.csv(unz(filepath, "languoid.csv"), header = TRUE, encoding = "UTF-8")
  colnames(glottologdata) <- base::tolower(colnames(glottologdata))
  glottologdata$bookkeeping <- ifelse(glottologdata$bookkeeping == "True", TRUE, FALSE)
  message("Glottolog data downloaded. This is the most recent version available from www.glottolog.org.")
  invisible(glottologdata)
}

#' Download glottolog data from zenodo, and select relevant data from cldf data
#'
#' @return
#' @export
#' @keywords internal
glottolog_download_cldf <- function(){
  glottolog_download_zenodo()
  glottolog_loadlocal()
}

#' Download most recent version of glottolog from zenodo (cldf format)
#'
#' @return
#' @export
#' @keywords internal
glottolog_download_zenodo <- function(){
  base_url <-  "https://zenodo.org/api/records/3260727" # Newest version is always uploaded here!
  req <- curl::curl_fetch_memory(base_url)
  content <- RJSONIO::fromJSON(rawToChar(req$content))
  url <- content$files[[1]]$links[[1]]
  filename <- base::basename(url)
  filepath <- glottofiles_makepath(filename)
  exdir <- glottofiles_makedir(tools::file_path_sans_ext(filename))

  utils::download.file(url = url, destfile = filepath ) # downloads and overwrites (i.e. changes date)
  utils::unzip(zipfile = filepath, exdir = exdir)
  message(paste0("Glottolog data downloaded (glottolog ", content$metadata$version,"). This is the most recent version available from https://zenodo.org/record/3260727)") )
}

#' Load locally stored glottolog data
#'
#' @return
#' @export
#' @keywords internal
glottolog_loadlocal <- function(){
  exdir <- glottofiles_makedir(paste0("glottolog-cldf-v", glottolog_version_local()))
  cldf_metadata <- base::list.files(exdir, pattern = "cldf-metadata.json", recursive = TRUE)
  mdpath <- normalizePath(file.path(exdir, cldf_metadata))
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
    dplyr::arrange(lang_id)

  colnames(glottologdata)[which(colnames(glottologdata) == "lang_id")] <- "id"
  glottologdata <- base::subset(glottologdata, select = -c(glottocode, language_id))
  invisible(glottologdata)
}
