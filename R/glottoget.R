#' Get glottodata from local path or online global databases
#'
#' Load locally stored glottodata, download databases from online sources, or load built-in demo data
#'
#' @param glottodata options are:
#' \itemize{
#' \item A filepath to locally stored glottodata or glottosubdata with file extension (.xlsx .xls .gpkg
#' .shp). See also: options meta and simplify.
#' \item "glottobase" - Default option, an spatially enhanced version of \href{https://glottolog.org/}{glottolog}. See
#' \link{glottobooster} for details. If glottodata = NULL, "glottobase" will be loaded.
#' \item "wals" - This is a spatially enhanced version of \href{https://wals.info/}{WALS}.
#' \item "dplace" - This is a spatially enhanced version of \href{https://d-place.org/}{D-PLACE}.
#' \item "glottolog" - This is a restructured (non-spatial) version of \href{https://glottolog.org/}{glottolog}.
#' \item "glottospace" - A simple dataset with glottocodes and a geometry column. This
#' is a subset of all languages in \href{https://glottolog.org/}{glottolog} with
#' spatial coordinates.
#' \item "demodata" - Built-in artificial glottodata (included for demonstration and testing).
#' \item "demosubdata" - Built-in artificial glottosubdata (included for demonstration and testing)
#' }
#' @param meta In case 'glottodata' is demodata/demosubdata: by default, meta sheets are not loaded. Use meta=TRUE if you want to include them.
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
#' @family <glottodata>
#'
#' @noRd
#' @seealso glottosave
glottoget_path <- function(filepath = NULL){

  # metasheets <- names(glottocreate_metatables())

  if(tools::file_ext(filepath) == "xlsx" | tools::file_ext(filepath) == "xls"){
    sheetnames <- readxl::excel_sheets(filepath)

  glottodata <- base::lapply(X = sheetnames,
                         FUN = readxl::read_excel, path = filepath)
  names(glottodata) <- sheetnames

    } else if(tools::file_ext(filepath) == "csv"){
      glottodata <- utils::read.csv(filepath, header = TRUE, encoding = "UTF-8")
    } else if(tools::file_ext(filepath) == "gpkg" | tools::file_ext(filepath) == "shp"){
      glottodata <- sf::st_read(dsn = filepath)
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
#' @param name Name of dataset "glottolog", "wals" or "dplace"
#' @param url Optional url
#'
#' @noRd
#'
glottoget_remotemeta <- function(name = NULL, url = NULL){
  rlang::check_installed("jsonlite", reason = "to use `glottoget_remotemeta()`")
  rlang::check_installed("xml2", reason = "to use `glottoget_remotemeta()`")
  if(is.null(name) & !is.null(url)){
    base_url <- url
  } else if(tolower(name) == "glottolog"){
    # Newest version is always uploaded here!
    base_url <- "https://zenodo.org/api/records/3260727"
  } else if(tolower(name) == "wals"){
    # Newest version is always uploaded here!
    base_url <- "https://zenodo.org/api/records/3606197"
  } else if(name == "dplace" | name == "d-place"){
    base_url <- "https://zenodo.org/api/records/3935419"
  } else if(!is.null(name) ){
    stop("Unable to download data from Zenodo. Unrecognized name argument. ")
  }

  remote <- suppressWarnings(jsonlite::stream_in(url(base_url)))

    version <- remote$metadata$version
    btime <- utils::timestamp()
    citation <- xml2::xml_text(xml2::read_html(charToRaw(remote$metadata$description), encoding = "UTF-8"))

    # v <- paste0(c("Version: ", version), collapse = "" )
    # c <- paste0(c("Citation: ", gsub(pattern = "\n", replacement = " ", x = citation)), collapse = "" )
    # b <- paste0(c("Built time: ", btime), collapse = "" )
    #
    # paste(c(v,c,b),  sep = "\n")

    metainfo <- paste0(c("version: ", version, "\n\n\n", citation, "\n\n", btime),  collapse = "")

    paste0("\\note{", metainfo, "}")

}



#' Download database from Zenodo
#'
#' @param name Name of a dataset, either glottolog, wals or dplace
#' @param dirpath Path to directory where files should be stored
#' @param url Zenodo url, something like this: "https://zenodo.org/api/records/3260727"
#'
#' @noRd
glottoget_zenodo <- function(name = NULL, url = NULL, dirpath = NULL){
  rlang::check_installed("jsonlite", reason = "to use `glottoget_zenodo()`")
  if(is.null(name) & !is.null(url)){
    base_url <- url
  } else if(tolower(name) == "glottolog"){
    # Newest version is always uploaded here!
    base_url <- "https://zenodo.org/api/records/3260727"
  } else if(tolower(name) == "wals"){
    # Newest version is always uploaded here!
    base_url <- "https://zenodo.org/api/records/3606197"
  } else if(name == "dplace" | name == "d-place"){
    base_url <- "https://zenodo.org/api/records/3935419"
  } else if(!is.null(name) ){
    stop("Unable to download data from Zenodo. Unrecognized name argument. ")
  }

  if(is.null(dirpath)){
    dirpath <- tempdir(check = TRUE)
    # if(dir.exists(dirpath)){unlink(x = dirpath, recursive = TRUE)}
  } else {
    if(dir.exists(dirpath)){stop("Directory already exists, please choose a different location.")}
    }

  remote <- suppressWarnings(jsonlite::stream_in(url(base_url)))
  url <- remote$files[[1]]$links[[1]]

  filepath <- file.path(tempfile())
  utils::download.file(file.path(url), destfile = filepath)
  utils::unzip(zipfile = filepath, exdir = dirpath)

  version <- remote$metadata$version

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
#' @param name Name of a dataset, either glottolog, wals or dplace
#' @param valuenames Do you want to add names instead of codes?
#'
#' @noRd
glottoget_cldf <- function(dirpath, name, valuenames = FALSE){
  if(!dir.exists(dirpath)){stop("Directory not found.")}

  cldf_metadata <- base::list.files(dirpath, pattern = "-metadata.json", recursive = TRUE)
  cldfid <- grep(pattern = tolower(name), x = tolower(cldf_metadata))
  mdpath <- normalizePath(file.path(dirpath, cldf_metadata[[cldfid]]))
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

  if(valuenames == TRUE) {    # Join values to codes by code_id
    codes <- normalizePath(file.path(mddir, "codes.csv"))
    codes <- utils::read.csv(codes, header = TRUE, encoding = "UTF-8")
    values <- values %>% dplyr::left_join(codes, by = c("code_id" = "ID") )
    values$value <- NULL
    values <- dplyr::rename(.data = values, value = .data$Name)
  }

  valsel <- values[, c("lang_id", "parameter_id", "value")]

  # Check duplicates:
  duplo <- valsel %>%
    dplyr::group_by(.data$lang_id, .data$parameter_id) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(.data$n > 1L)

  if(nrow(duplo) == 0){
    valselpiv <- tidyr::pivot_wider(data = valsel, names_from = "parameter_id", values_from = "value")
  } else {
    message(paste0("For ", length(unique(duplo$lang_id)), " of the ", length(unique(valsel$lang_id)), " languages, a parameter has multiple values. These are summarized by taking the mean"))
    valselpiv <- tidyr::pivot_wider(data = valsel, names_from = "parameter_id", values_from = "value",
                                    values_fn = mean)
  }

  data <- dplyr::left_join(languoids, valselpiv, by = "lang_id")
  data <- base::subset(data, select = c("glottocode", params))
  data <- data[!purrr::is_empty(data$glottocode) & data$glottocode != "", ]

  data <- glottojoin_base(data)
  invisible(data)


}


