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

  out <- glottoget_zenodo(
    name = "worldatlas",
    subfolder = file.path("cldf", version),
    file = paste0(polygon_set, ".geojson"),
    download = download,
    dirpath = dirpath
  )

  return(out)
}

