is_raster <- function(object){
  (class(object)[1] == "RasterLayer") | (class(object)[1] == "RasterStack")
}

is_sf <- function(object){
  class(object)[1] == "sf"
}

