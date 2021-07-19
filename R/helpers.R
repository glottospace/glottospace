## From https://geocompr.robinlovelace.net/reproj-geo-data.html
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
#' @export
#'
#' @examples
contransform_lonlat <- function(data){
  if(!sf::st_is_longlat(data)) {
    data <- sf::st_transform(x = data, crs = "EPSG:4326")
  }
  return(data)
}

contransform_distmat <- function(dist){
  distmat <- as.matrix(dist)
}



#' Create empty distance matrix
#'
#' @param names
#'
#' @return
#' @export
#'
#' @examples
#' gs_emptydistmat(names = glottocodes)
gs_emptydistmat <- function(names){
  outputmat <- matrix(data = NA,
                      nrow = length(names),
                      ncol = length(names),
                      dimnames = list(names, names))
  return(outputmat)
}


as.ordfact <- function(x = NULL, levels = NULL){ # alternatively, use: https://forcats.tidyverse.org/
  dtf <- as.factor(as.matrix(x))
  lvl <- unlist(strsplit(x = levels, split = '[,;]+'))
  dtot <- factor(x = dtf, levels = lvl, ordered = TRUE)
  df <- as.data.frame(dtot)
  return(df)
}


