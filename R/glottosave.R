#' Save any geoglot/glot object in relevant format
#'
#' If no file extention is provided, a sensible default is chosen.
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
  if((class(object) == "sf")[1]){
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
