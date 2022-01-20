

# Small helper functions only! Major functions migrated to geospace --------



#' Calculate EPSG code from lonlat
#'
#' From https://geocompr.robinlovelace.net/reproj-geo-data.html
#' @param lonlat
#'
#' @return
#' @noRd
#'
lonlat2utm = function(lonlat) {
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}

#' Conditionally transforms coordinates to lonlat
#'
#' If CRS of a spatial object is not longitude latitude it will be converted.
#'
#' @param data Spatial object with any crs
#'
#' @return Spatial object transformed to lonlat
#' @noRd
#'
contransform_lonlat <- function(data){
  # Split in two functions: is_lonlat and is_lonlattransform
  if(!is_lonlat(data)) {
    # raster::isLonLat
    data <- sf::st_transform(x = data, crs = "EPSG:4326")
  }
  return(data)
}

is_lonlat <- function(object){
  if(is.na(sf::st_is_longlat(object))){stop("No coordinate reference system!")}
  sf::st_is_longlat(object)
}

#' Conditionally transforms dist object to distance matrix
#'
#' If dist object is not a distance matrix it will be converted.
#'
#' @param dist dist object
#'
#' @return distance matrix
#' @noRd
#'
contransform_distmat <- function(dist){
  if(any(class(dist) == "dist")){
    distmat <- as.matrix(dist)
  } else if (any(class(dist) == "matrix")){
    distmat <- dist
  }
}

#' Check whether crs of two spatial objects are identical
#'
#' @param x spatial object
#' @param y spatial object
#'
#' @return TRUE/FALSE
#' @noRd
#'
identicalcrs <- function(x, y){
  base::identical(sf::st_crs(x), sf::st_crs(y))
}

#' Check whether an object is of class raster
#'
#' @param object any object
#'
#' @return TRUE/FALSE
#' @noRd
#'
is_raster <- function(object){
  (class(object)[1] == "RasterLayer") | (class(object)[1] == "RasterStack")
}

#' Check whether an object is of class sf
#'
#' @param object any object
#'
#' @return TRUE/FALSE
#' @noRd
#' @export
#'
is_sf <- function(object){
  class(object)[1] == "sf"
}



#' Check whether crs is specified for an object
#'
#' @param object any object
#'
#' @return TRUE/FALSE
#' @noRd
#'
glottocheck_crsmissing <- function(object){
  is.na(sf::st_crs(object))
}


#' Check whether the geometry of a spatial object is POINT
#'
#' Check whether geometry type is one of: POINT, MULTIPOINT, or a combination thereof
#'
#' @param object any object
#'
#' @return TRUE/FALSE
#' @noRd
#'
is_point <- function(object){
  all(unique(sf::st_geometry_type(object)) %in% c("POINT", "MULTIPOINT"))
}


#' Check whether the geometry of a spatial object is POLYGON
#'
#' Check whether geometry type is on of: POLYGON, MULTIPOLYGON, or a combination thereof.
#'
#' @param object any object
#'
#' @return TRUE/FALSE
#' @noRd
#'
is_polygon <- function(object){
  all(unique(sf::st_geometry_type(object)) %in% c("POLYGON", "MULTIPOLYGON"))
}

#' Expand bounding box
#'
#' @param bbox original bounding box
#' @param f expansion factor
#'
#' @return Expanded bounding box
#' @noRd
#'
bbox_expand <- function(bbox, f = 0.2){

  xrange <- bbox$xmax - bbox$xmin # range of x values
  yrange <- bbox$ymax - bbox$ymin # range of y values

  bbox[1] <- bbox[1] - (f * xrange) # xmin - left
  bbox[3] <- bbox[3] + (f * xrange) # xmax - right
  bbox[2] <- bbox[2] - (f * yrange) # ymin - bottom
  bbox[4] <- bbox[4] + (f * yrange) # ymax - top
  bbox
}
