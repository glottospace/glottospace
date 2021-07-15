is_raster <- function(object){
  (class(object)[1] == "RasterLayer") | (class(object)[1] == "RasterStack")
}

is_sf <- function(object){
  class(object)[1] == "sf"
}

checkdata_crsmissing <- function(object){
  is.na(sf::st_crs(object))
}
