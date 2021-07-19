#' Calculates geographic distances between points, as the bird flies (great circle distance)
#'
#' Great circle distances (Haversine distances) are calculated between all points.
#'
#' @param points Spatial point object
#' @param label Column name or number indicating how objects should be labeled
#'
#' @return dist object with distances (km) between all points
#' @export
#'
#' @examples
#' pointdist_bird(points = points, label = "glottocode")
pointdist_bird <- function(points, label){

      points <- contransform_lonlat(points)
      geodist <- sf::st_distance(x = points)
      geodist <- geodist %>% units::set_units(km) # convert with set_units because geodist %>% "/"(1000) would not keep the correct units class
      rownames(geodist) <- points[, label, drop = T]
      colnames(geodist) <- points[, label, drop = T]
      geodist <- round(geodist)

  return(geodist)
}

#' Calculate distance from each point to nearest line, as the bird flies (great circle distance)
#'
#' Great circle distances (Haversine distances) are calculated between points an nearest line (e.g. river, road, etc. ).
#'
#' @param points Spatial point object
#' @param lines Spatial line object
#' @param label Column name or number indicating how objects should be labeled
#'
#' @return dist object with distances from each point (km) to the nearest line.
#' @export
#'
#' @examples
#' nearestline_bird(points = points, lines = rivers, label = "glottocode")
#' nearestline_bird(points = points, lines = roads, label = "glottocode")
nearestline_bird <- function(points, lines, label){
  points <- contransform_lonlat(points)
  lines <- contransform_lonlat(lines)
  nearest <- st_nearest_feature(x = points, y = lines)
  geodist <- st_distance(x = points, y = lines[nearest,], by_element = TRUE)
  geodist <- geodist %>% set_units(km)
  geodist <- round(geodist)
  # if(return == "sf"){
  #   points[ , "dist"] <- geodist
  #   geodistsf <- points
  # } else{names(geodist) <- points[, "name", drop = T]
  # }

  return(geodist)
}


gs_geodist <- function(points, lines, label = "name"){
  # Perhaps split 'return' into 'class' and 'summary'???


  fun <- tolower(str_replace_all(fun, "[[:punct:]]", " ")  )

  if(fun %in% c("lc", "wolf", "least cost", "least cost distance")){
    # TO ADD:
    # gdistance R package: Van Etten 2017
    # topoDistance R package; Wang 2020
    # Least Cost Topographic Path: Taking into account both hanitat suitability and topography.
    # Least Cost Path: Only habitat suitability
    # Shortest topographic path: only topography.
    # Topographic distances account for the additional distance, beyond horizontal distance, imposed by topographic relief and, therefore, capture the full overland distance an organism must move between geographic locations.
  }

  if(fun %in% c("river", "fish", "riverdist", "river distance")){
    # This is distance along river, not to river (will be implemented in gs_datageo)
    # TO ADD:
    # https://cran.r-project.org/web/packages/riverdist/vignettes/riverdist_vignette.html
  }

  if(fun %in% c("resistance", "drunkard", "random walk", "commute distance")){
    # implemented in gdistance
    # TO ADD:
    # https://robbymarrotte.weebly.com/blog/running-circuitscape-in-r-windows-os
  }

  if(return == "units"){
    out <- geodist
    message("Matrix of class 'units' returned. Distances are in km.")
  }
  if(return == "sf"){
    out <- geodistsf
    message("Class 'sf' returned. Distances are in km.")
  }
  if(return == "dist" | return == "distmat"){
    out <- as.dist(geodist)
    message("Distance matrix returned (default). Distances are in km.")
  } else if(return == "matrix"){
    out <- as.matrix(geodist)
    message("Matrix returned. Distances are in km.")
  } else if(return == "graph"){
    out <- reshape2::melt(as.matrix(geodist), na.rm = TRUE)
    colnames(out)[1] <- "lang1"
    colnames(out)[2] <- "lang2"
    colnames(out)[3] <- "dist"
    message("Graph returned")
  }

}

#' Calculate mean distance from one features to all other features
#'
#' @param dist \code{dist} object
#'
#' @return
#' @export
#'
#' @examples
meandist <- function(dist){
  totdist <- rowSums(as.matrix(dist))
  meandist <- totdist / (nrow(dist) - 1)
  return(meandist)
}

#' Count number of points within a radius
#'
#' @param dist dist object
#' @param r search radius (in same units as \code{dist} object)
#'
#' @return Numeric vector with distances
#' @export
#'
#' @examples
#' countwithinradius(dist)
countwithinradius <- function(dist, r){
  apply(dist, 1, function(x) {
    sum(x < r) - 1 # Subtract 1 to exclude the point itself
  })
}

#' Get distance to nearest features
#'
#' @param dist dist object to obtain distances from
#' @param n The n nearest features for which distances should be obtained, default (n = 1) is only distance to nearest neighbor.
#'
#' @return numeric vector with distances if only nearest distance is requested (n = 1) and a data.frame otherwise.
#' @export
#'
#' @examples
#' nearestdistance(dist)
nearestdistance <- function(dist, n = 1){
  nnd <- apply(dist, 1, function(x) { return(sort(x, partial = 2)[c(2:(n+1))]) }) # start at 2, because 1 is the point itself.
  if(n ==1 ){
    return(nnd)
  } else if(n != 1){
    nnd <- t(nnd)
    nnddf <- as.data.frame(nnd)
    colnames(nnddf) <- paste0("nndist", seq(1,n))
    nnddf <- tibble::rownames_to_column(nnddf, var = "id")
    return(nnddf)
  }
}

#' Get index of nearest feature(s)
#'
#' @param dist dist object to obtain nearest features from
#' @param n For how many features should indices be obtained, default (n = 1) is only distance to nearest neighbor.
#'
#' @return numeric vector with indices if n = 1 and a data.frame with indices otherwise.
#' @export
#'
#' @examples
#' ind <- nearestfeature(dist = dist, n = 1)
#' ind2 <- nearestfeature(dist = dist, n = 2)
#' data.frame(point = rownames(ind), neighbor = rownames(ind)[ind$nnids1])
#' cbind(rownames(ind),rownames(ind)[ind$nnids1])
nearestfeature <- function(dist, n = 1){
 nns <- apply(dist, 1, function(x) { order(x, decreasing=FALSE)[c(2:(n+1))] })
 if(n ==1){
   return(nns)
 } else if(n != 1){
   nns <- t(nns)
   nnsdf <- as.data.frame(nns)
   colnames(nnsdf) <- paste0("nnids", seq(1,n))
   nnsdf <- tibble::rownames_to_column(nnsdf, var = "id")
   return(nnsdf)
 }
}





