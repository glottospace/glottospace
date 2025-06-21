#' Get World Atlas data from Zenodo
#'
#' This function loads World Atlas polygon data from local path or downloads it via Zenodo.
#' Version options are "contemporary" or "traditional", and polygon_set options are "features", "languages", or "families".
#'
#' @param version One of "contemporary" or "traditional".
#' @param polygon_set One of "features", "languages", or "families".
#' @param download Logical. If TRUE, downloads the data from Zenodo.
#' @param dirpath Optional. Path to local data directory.
#'
#' @return An `sf` object with the requested World Atlas polygon data.
#' @export
#'
#' @examples
#' \donttest{
#' glottoget_worldatlas()
#' }
glottoget_worldatlas <- function(version = c("contemporary", "traditional"),
                                 polygon_set = c("features", "languages", "families"),
                                 download = FALSE,
                                 dirpath = NULL) {
  version <- match.arg(version, choices = c("contemporary", "traditional"))
  polygon_set <- match.arg(polygon_set, choices = c("features", "languages", "families"))

  if (is.null(download)) {
    download <- FALSE
  }

  if (download == FALSE & is.null(dirpath)) {
    stop("Either set download = TRUE or provide dirpath to local data.")
  } else if (download == TRUE) {
    invisible(readline(prompt = "Are you sure you want to download World Atlas data?\nPress [enter] to continue"))
    dirpath <- glottoget_zenodo(name = "worldatlas", dirpath = dirpath)
  }

  out <- glottoget_worldatlas_loadlocal(version = version,
                                        polygon_set = polygon_set,
                                        dirpath = dirpath)
  return(out)
}


#' Load locally stored World Atlas data
#'
#' @noRd
glottoget_worldatlas_loadlocal <- function(version = "contemporary",
                                           polygon_set = "features",
                                           dirpath) {
  if (!dir.exists(dirpath)) stop("Directory not found: ", dirpath)

  geojson_path <- file.path(dirpath, "cldf", version, paste0(polygon_set, ".geojson"))

  if (!file.exists(geojson_path)) {
    stop("GeoJSON file not found: ", geojson_path)
  }

  out <- sf::read_sf(geojson_path)
  return(out)
}
