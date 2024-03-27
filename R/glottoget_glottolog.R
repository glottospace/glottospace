#' Get glottolog data
#'
#' This function loads the Glottolog data that is distributed with glottospace or optionally downloads it.
#' type ?glottolog for more information about the version, and how to cite the data. #'
#'
#' @noRd
#'
#' @examples
#' \donttest{
#' glottoget_glottolog()
#' }
glottoget_glottolog <- function(download = NULL, dirpath = NULL){
  if(is.null(download)){
    download <- FALSE
  }
  if(download == FALSE & is.null(dirpath) ){
    out <- glottospace::glottolog
  } else if(download == FALSE & !is.null(dirpath)){
    out <- glottoget_glottologloadlocal(dirpath = dirpath)
  } else if(download == TRUE){
    out <- glottoget_glottologdownload(dirpath = dirpath)
  }
  return(out)
}

#' Download glottolog data
#'
#' @noRd
#'
glottoget_glottologdownload <- function(dirpath = NULL){
  invisible(readline(prompt="Are you sure you want to download Glottolog data? \n Press [enter] to continue"))
  dirpath <- glottoget_zenodo(name = "glottolog", dirpath = dirpath)
  glottoget_glottologloadlocal(dirpath = dirpath)
}

#' Load locally stored glottolog data
#'
#' @param dirpath Path to directory where glottolog cldf data is stored
#'
#' @importFrom rlang .data
#' @noRd
glottoget_glottologloadlocal <- function(dirpath){
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

  glottologdata <- {if("level" %nin% colnames(languoids)){dplyr::left_join(languoids, levels, by = "lang_id")}else{languoids}} %>%
    dplyr::left_join(category, by = "lang_id") %>%
    dplyr::left_join(classification, by = "lang_id") %>%
    dplyr::arrange(.data$lang_id)

  colnames(glottologdata)[which(colnames(glottologdata) == "lang_id")] <- "id"
  glottologdata <- glottologdata %>% dplyr::select(-.data$glottocode, -.data$language_id)
  invisible(glottologdata)
}
