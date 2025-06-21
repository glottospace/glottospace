#' Get World Atlas data from Zenodo
#'
#' @param version One of "contemporary" or "traditional".
#' @param polygon_set One of "features", "languages", or "families".
#' @param download Logical. If TRUE, downloads from Zenodo. Default is FALSE.
#' @param dirpath Optional. Local directory path.
#' @return sf object with spatial data.
#' @export
glottoget_worldatlas <- function(version = c("contemporary", "traditional"),
                                 polygon_set = c("features", "languages", "families"),
                                 download = FALSE,
                                 dirpath = NULL) {
  version <- match.arg(version)
  polygon_set <- match.arg(polygon_set)

  if (download) {
    # call glottoget_zenodo to download the full dataset
    glottoget_zenodo(name = "worldatlas", dirpath = dirpath)
  }

  if (is.null(dirpath)) {
    stop("Either set download = TRUE or provide dirpath to local data.")
  }

  # Construct path to specific GeoJSON file
  geojson_path <- file.path(dirpath, "cldf", version, paste0(polygon_set, ".geojson"))

  if (!file.exists(geojson_path)) {
    stop("GeoJSON file not found at: ", geojson_path)
  }

  out <- sf::read_sf(geojson_path)
  return(out)
}
