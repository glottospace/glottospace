#' Save any geoglot/glot object in relevant format
#'
#' If no file extention is provided, a sensible default is chosen. Dynamic maps
#' (tmap) are saved in .html format, static maps (tmap) are saved as .png.
#' Spatial data (sf) are saved as geopackage (.GPKG) by default, but .shp is
#' also possible.
#'
#' @param object
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
glottosave <- function(object = NULL, filename = NULL){
  # dynamic/static maps
  # empty glottospace object
  # spatial object as GPKG

  if((class(object) == "tmap")[1]){
    filename <- sub(pattern = "(.*)\\..*$", replacement = "\\1", filename)
    tmap::tmap_save(object, filename = filename)
  }
  if( is_sf(object) ){
    # if no file extension: gpkg
    if(tools::file_ext(filename) == ""){
      sf::st_write(obj = object, dsn = paste0(filename, ".gpkg"),
               append = FALSE)
    } else {
      sf::st_write(obj = object, dsn = filename,
               append = FALSE)
    }
  }
}

#' Load glottodata
#'
#' @param filename
#' @aliases glottoread
#' @return
#' @export
#'
#' @examples
#' glottoload(path)
glottoload <- function(filename){
  # should this be integrated with get_glottodata?
  if(tools::file_ext(filename) == ".gpkg" | tools::file_ext(filename) == ".shp"){
  sf::st_read(dsn = filename)
  }
}
