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
#' \item "wals" - This is a spatially enhanced version of \href{https://wals.info/}{WALS}.
#' \item "dplace" - This is a spatially enhanced version of \href{https://d-place.org/}{D-PLACE}.
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
#' @param url Zenodo url, something like this: "https://zenodo.org/api/records/3260727"
#'
#' @family <glottodata>
#' @return A glottodata or glottosubdata object (a data.frame or list, depending on which glottodata is requested)
#' @export
#' @examples
#' \donttest{
#' glottoget("glottolog")
#' }
glottoget <- function(glottodata = NULL, meta = FALSE, download = FALSE, dirpath = NULL, url = NULL){
  if(!is.null(url)){
    glottoget_zenodo(url = url, dirpath = dirpath)
  } else if(is.null(glottodata)){
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
  } else if(glottodata == "dplace"){
    glottodata <- glottoget_dplace(download = download)
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



#' Get metadata of remote cldf databases
#'
#' @param name Name of dataset "wals" or "glottolog"
#' @param url Optional url
#'
#' @noRd
#'
glottoget_remotemeta <- function(name = NULL, url = NULL){

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

  remote <- suppressWarnings(jsonlite::stream_in(url(base_url)))

    version <- remote$metadata$version
    now <- utils::timestamp()
    citation <- xml2::xml_text(xml2::read_html(charToRaw(remote$metadata$description), encoding = "UTF-8"))

    metainfo <- paste0(c("version: ", version, "\n\n\n", citation, "\n\n", now),  collapse = "")

    paste0("\\note{", metainfo, "}")

}



#' Download database from Zenodo
#'
#' @param name Name of a dataset, either wals or glottolog
#' @param dirpath Path to directory where files should be stored
#' @param url Zenodo url, something like this: "https://zenodo.org/api/records/3260727"
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
  } else if(name == "dplace"){
    base_url <- "https://zenodo.org/api/records/3935419"
  } else if(!is.null(name) ){
    stop("Unable to download data from Zenodo. Unrecognized name argument. ")
  }

  if(is.null(dirpath)){
    dirpath <- tempdir()
    if(dir.exists(dirpath)){unlink(x = dirpath, recursive = TRUE)}
  } else {
    if(dir.exists(dirpath)){stop("Directory already exists, please choose a different location.")}
    }

  req <- curl::curl_fetch_memory(base_url)
  content <- RJSONIO::fromJSON(rawToChar(req$content))
  url <- content$files[[1]]$links[[1]]

  filepath <- tempfile()
  utils::download.file(file.path(url), destfile = filepath)
  utils::unzip(zipfile = filepath, exdir = dirpath)

  version <- content$metadata$version

  if(!is.null(name)){
  if(tolower(name) == "glottolog"){
    message(paste0("Glottolog data downloaded (glottolog ", version,"). This is the most recent version available from ", base_url) )
  } else if(tolower(name) == "wals"){
    message(paste0("WALS data downloaded (wals-", version,"). This is the most recent version available from ", base_url) )
  } else if(tolower(name) == "dplace"){
    message(paste0("D-PLACE data downloaded (", version,"). This is the most recent version available from ", base_url) )
  }
  }
invisible(dirpath)

}

#' Load locally stored cldf data
#'
#' @param dirpath Path to directory where cldf data is stored
#'
#' @noRd
glottoget_cldfloadlocal <- function(dirpath, valuenames = NULL, paramnames = NULL){
  if(!dir.exists(dirpath)){stop("Directory not found.")}
  if(is.null(valuenames)){valuenames <- TRUE}
  if(is.null(paramnames)){paramnames <- FALSE}

  cldf_metadata <- base::list.files(dirpath, pattern = "-metadata.json", recursive = TRUE)
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
  params <- unique(values$parameter_id)
  if(valuenames == FALSE){
    values <- tidyr::pivot_wider(data = values, names_from = "parameter_id", values_from = "value")
  } else {    # Join values to codes by code_id
    codes <- normalizePath(file.path(mddir, "codes.csv"))
    codes <- utils::read.csv(codes, header = TRUE, encoding = "UTF-8")
    values <- values %>% dplyr::left_join(codes, by = c("code_id" = "ID") )
    values <- tidyr::pivot_wider(data = values, names_from = "parameter_id", values_from = "Name")
  }

  # Create empty data frame to store results
  langs <- unique(values$lang_id)
  langvals <- data.frame(matrix(ncol = ncol(values), nrow = length(langs)))
  colnames(langvals) <- colnames(values)
  langvals[,"lang_id"] <- langs

  for(i in seq_along(langs)){
    lang <- langs[[i]]
    langtb <- values[values[,"lang_id"] == lang, params]
    langvals[langvals["lang_id"] == lang, params] <- apply(X = langtb, MARGIN = 2, FUN = nonna, max1 = TRUE)
  }

  data <- languoids %>% dplyr::left_join(langvals, by = "lang_id")
  data <- base::subset(data, select = c("glottocode", params))
  data <- data[!purrr::is_empty(data$glottocode) & data$glottocode != "", ]

  if(paramnames == TRUE){# Add parameter labels
    parameters <- normalizePath(file.path(mddir, "parameters.csv"))
    parameters <- utils::read.csv(parameters, header = TRUE, encoding = "UTF-8")
    colnames(data)[-1] <- parameters$Name[match(colnames(data), parameters$ID )][-1]
  }

  data <- glottojoin_base(data)
  invisible(data)


}


