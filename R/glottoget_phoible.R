#' Get phoible data
#'
#' This function loads the PHOIBLE data that is distributed with PHOIBLE or optionally downloads it.
#' type ?phoible for more information about the version, and how to cite the data.
#'
#' @noRd
#'
#' @examples
#' \donttest{
#' glottoget_phoible()
#' }
glottoget_phoible <- function(download = NULL, dirpath = NULL){
  if(is.null(download)){
    download <- FALSE
  }
  if(download == FALSE & is.null(dirpath) ){
    out <- glottospace::phoible
    } else if(download == FALSE & !is.null(dirpath)){
    out <- glottoget_phoibleloadlocal(dirpath = dirpath)
  } else if(download == TRUE){
    out <- glottoget_phoibledownload(dirpath = dirpath)
  }
  return(out)
}


#' Download phoible data
#'
#' @noRd
#'
glottoget_phoibledownload <- function(dirpath = NULL){
  invisible(readline(prompt="Are you sure you want to download PHOIBLE data? \n Press [enter] to continue"))
  dirpath <- glottoget_zenodo(name = "phoible", dirpath = dirpath)
  glottoget_phoibleloadlocal(dirpath = dirpath)
}


#' Load locally stored phoible data
#'
#' @param dirpath Path to directory where phoible cldf data is stored
#'
#' @importFrom rlang .data
#' @noRd
glottoget_phoibleloadlocal <- function(dirpath){
  if(!dir.exists(dirpath)){stop("Directory not found.")}
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

  lang_id <- NULL
  parameter_id <- NULL
  value <- NULL

  values <- subset(values, select=c(lang_id, parameter_id, value))
  values <- dplyr::distinct(values)
  values <- tidyr::pivot_wider(data = values, names_from = "parameter_id", values_from = "value")

  for (idx in 1:ncol(values)){
    values[, idx] <- tidyr::replace_na(values[, idx, drop = T], "absent")
  }

  phoibledata <- languoids %>% dplyr::left_join(values, by = "lang_id") %>%
    # dplyr::left_join(category, by = "lang_id") %>%
    # dplyr::left_join(classification, by = "lang_id") %>%
    dplyr::arrange(.data$lang_id)

  colnames(phoibledata)[which(colnames(phoibledata) == "lang_id")] <- "id"
  phoibledata <- phoibledata %>% dplyr::select(-.data$glottocode)
  invisible(phoibledata)
}
