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
    out <- glottospace::phoible_raw
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

  # values <- subset(values, select=c(lang_id, parameter_id, value, contribution_id))
  values <- values[, c("lang_id", "parameter_id", "value", "contribution_id")]
  values <- dplyr::distinct(values)
  values <- tidyr::pivot_wider(data = values, names_from = "parameter_id", values_from = "value")

  for (idx in 1:ncol(values)){
    values[, idx] <- tidyr::replace_na(values[, idx, drop = T], "absent")
  }

  phoibledata <- languoids %>% dplyr::full_join(values, by = "lang_id") %>%
    # dplyr::left_join(category, by = "lang_id") %>%
    # dplyr::left_join(classification, by = "lang_id") %>%
    dplyr::arrange(.data$lang_id)

  colnames(phoibledata)[which(colnames(phoibledata) == "lang_id")] <- "id"
  phoibledata <- phoibledata %>% dplyr::select(-.data$glottocode)
  invisible(phoibledata)
}




#' Title
#'
#' @param phoible_data A non-spatial phoible dataset
#'
#' @return an sf object
#' @export
#'
#' @examples
#' phoible_sf <- phoible_param_sf(glottospace::phoible_raw)
phoible_param_sf <- function(phoible_data){
  param_idx <- colnames(phoible_data) |>
    sapply(
      FUN = function(x){
        nchar(x) == 32
      }
    )
  param_ids <- colnames(phoible_data)[param_idx]

  data <- phoible_data[, c("longitude", "latitude", param_ids)] |>
    stats::na.omit()

  data_non_na_id <- data[, 3:ncol(data)] |>
    apply(MARGIN = 2,
          FUN = function(x){
            !all(x == "absent")
          }) |>
    unlist() |>
    which() |>
    names()

  data <- data[, c("longitude", "latitude", data_non_na_id)]

  geometry <- data[, 3:ncol(data)] |>
    apply(
      MARGIN = 2,
      FUN = function(x){
        data[which(x != "absent"), 1:2] |>
          as.matrix() |>
          sf::st_multipoint()
      }
    ) |>
    sf::st_sfc(crs = 4326)

  data_param_id <- as.matrix(colnames(data)[3:ncol(data)])

  colnames(data_param_id) <- "Parameter ID"

  data_sf <- sf::st_sf(data_param_id, geometry)

  data_sf
}













