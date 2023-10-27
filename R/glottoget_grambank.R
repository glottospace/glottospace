#' Get grambank data
#'
#' This function loads the Grambank data that is distributed with grambank or optionally downloads it.
#' type ?grambank for more information about the version, and how to cite the data. #'
#'
#' @noRd
#'
#' @examples
#' glottoget_grambank()
glottoget_grambank <- function(download = NULL, dirpath = NULL){
  if(is.null(download)){
    download <- FALSE
  }
  if(download == FALSE & is.null(dirpath) ){
    out <- glottospace::grambank
    } else if(download == FALSE & !is.null(dirpath)){
    out <- glottoget_grambankloadlocal(dirpath = dirpath)
  } else if(download == TRUE){
    out <- glottoget_grambankdownload(dirpath = dirpath)
  }
  return(out)
}


#' Download grambank data
#'
#' @noRd
#'
glottoget_grambankdownload <- function(dirpath = NULL){
  invisible(readline(prompt="Are you sure you want to download Grambank data? \n Press [enter] to continue"))
  dirpath <- glottoget_zenodo(name = "grambank", dirpath = dirpath)
  glottoget_grambankloadlocal(dirpath = dirpath)
}


#' Load locally stored grambank data
#'
#' @param dirpath Path to directory where grambank cldf data is stored
#'
#' @importFrom rlang .data
#' @noRd
glottoget_grambankloadlocal <- function(dirpath){
  if(!dir.exists(dirpath)){stop("Directory not found.")}
  cldf_metadata <- base::list.files(dirpath, pattern = "StructureDataset-metadata.json", recursive = TRUE)
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

  values <- subset(values, select=c(lang_id, parameter_id, value))
  values <- tidyr::pivot_wider(data = values, names_from = "parameter_id", values_from = "value")

  grambankdata <- languoids %>% dplyr::left_join(values, by = "lang_id") %>%
    # dplyr::left_join(category, by = "lang_id") %>%
    # dplyr::left_join(classification, by = "lang_id") %>%
    dplyr::arrange(.data$lang_id)

  colnames(grambankdata)[which(colnames(grambankdata) == "lang_id")] <- "id"
  grambankdata <- grambankdata %>% dplyr::select(-.data$glottocode)
  invisible(grambankdata)
}
