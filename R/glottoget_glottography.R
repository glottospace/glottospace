#' Download and process Glottography data from Zenodo
#'
#' This function downloads and loads GeoJSON spatial data for Glottography from Zenodo.
#' It supports polygon sets: 'features', 'languages', and 'families'.
#'
#' @param dirpath Directory where the data will be stored. If NULL, uses a temporary folder.
#' @param polygon_set One of "features", "languages", or "families".
#'
#' @return A `sf` object containing the polygon data.
#' @export
glottoget_glottography <- function(dirpath = NULL, polygon_set = c("features", "languages", "families")) {
  polygon_set <- match.arg(polygon_set)

  # Zenodo base URL for Glottography v1.0.0
  zenodo_base <- "https://zenodo.org/records/15287258/files"

  file_name <- paste0(polygon_set, ".geojson")
  download_url <- file.path(zenodo_base, file_name)

  if (is.null(dirpath)) {
    dirpath <- tempfile("glottoget_glottography")
  }
  if (!dir.exists(dirpath)) {
    dir.create(dirpath, recursive = TRUE)
  }

  file_path <- file.path(dirpath, file_name)

  # Inform user and download
  message("Downloading Glottography data from Zenodo: ", file_name)
  utils::download.file(download_url, destfile = file_path, mode = "wb")

  # Load as sf
  data <- sf::st_read(file_path, quiet = TRUE)

  return(data)
}

source("R/glottoget_glottography.R")

