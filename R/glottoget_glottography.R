#' Get glottography data
#'
#' This function loads the Glottography data that is distributed with glottospace or optionally downloads it.
#' @noRd
#'
#' @examples
#' \donttest{
#' glottoget_glottography()
#' }
glottoget_glottography <- function(download = NULL, dirpath = NULL){
  if(is.null(download)){
    download <- FALSE
  }
  if(download == FALSE & is.null(dirpath)){
    out <- glottospace::glottography
  } else if(download == FALSE & !is.null(dirpath)){
    out <- glottoget_glottographyloadlocal(dirpath = dirpath)
  } else if(download == TRUE){
    out <- glottoget_glottographydownload(dirpath = dirpath)
  }
  return(out)
}

#' Download glottography data
#'
#' @noRd
#'
glottoget_glottographydownload <- function(dirpath = NULL){
  invisible(readline(prompt="Are you sure you want to download Glottography data? \n Press [enter] to continue"))
  dirpath <- glottoget_zenodo(name = "glottography", dirpath = dirpath)
  glottoget_glottographyloadlocal(dirpath = dirpath)
}

#' Load locally stored glottography data
#'
#' @param dirpath Path to directory where glottography cldf data is stored
#'
#' @importFrom rlang .data
#' @noRd
glottoget_glottographyloadlocal <- function(dirpath){
  if(!dir.exists(dirpath)){stop("Directory not found.")}
  cldf_metadata <- base::list.files(dirpath, pattern = "cldf-metadata.json", recursive = TRUE)
  mdpath <- normalizePath(file.path(dirpath, cldf_metadata))
  mddir <- normalizePath(base::dirname(mdpath))
  
  # Load languages file
  languoids <- normalizePath(file.path(mddir, "languages.csv"))
  languoids <- utils::read.csv(languoids, header = TRUE, encoding = "UTF-8")
  colnames(languoids) <- base::tolower(colnames(languoids))
  colnames(languoids)[which(colnames(languoids) == "id")] <- "lang_id"
  
  # Load glottography-specific data file
  values <- normalizePath(file.path(mddir, "glottography_values.csv"))
  values <- utils::read.csv(values, header = TRUE, encoding = "UTF-8")
  colnames(values) <- base::tolower(colnames(values))
  colnames(values)[colnames(values) == "language_id"] <- "lang_id"
  values <- tidyr::pivot_wider(data = values, names_from = "parameter_id", values_from = "value")
  
  # Example for potential levels or categories (modify as needed for glottography-specific fields)
  levels <- values[!is.na(values$level), c("lang_id", "level")]
  category <- values[!is.na(values$category), c("lang_id", "category")]
  
  glottographydata <- dplyr::left_join(languoids, levels, by = "lang_id") %>%
    dplyr::left_join(category, by = "lang_id") %>%
    dplyr::arrange(.data$lang_id)
  
  colnames(glottographydata)[which(colnames(glottographydata) == "lang_id")] <- "id"
  invisible(glottographydata)
}
