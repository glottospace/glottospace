# > dist <- pointdist_bird(glottodata, "glottocode")
# > pointdist_bird(dist)
# Error in if (!sf::st_is_longlat(data)) { :
#     missing value where TRUE/FALSE needed
# Probably in contransform_lonlat

#' Pairwise distances between spatial objects
#'
#' This function calculates pairwise distances between many points, and distances between many points and many lines.
#' Different distance metrics are possible: least cost distance, river distance,
#'
#'
#' @param points Spatial points between which distances should be calculated
#'   (sf)
#' @param lines Optional, spatial lines to which distances should be calculated
#' @param fun function indicating how distances should be calculated. Default is
#'   "bird" (Haversine distance/great circle distance). Other options are:
#'   "leastcost" (least cost distance), "along" (along lines such as rivers or roads), and
#'   "resistance" (commute distance/random walk/drunkard)
#' @param label column name that should be used to label points.
#' @family <geodist>
#' @return
#' @export
#'
#' @examples
#' glottodata <- glottoget()
#' glottodataspace <- glottospace(glottodata)
#' geodist(points = glottodataspace)
geodist <- function(points, lines = NULL, fun = NULL, label = NULL){
  # Perhaps split 'return' into 'class' and 'summary'???
  if(is.null(label)){label <- "glottocode"}
  if(is.null(fun)){fun <- "bird"}

  if(fun %in% c("bird", "haversine", "greatcircle")){
    geodist <- pointdist_bird(points = points, label = label)

  }


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

  # if(return == "units"){
  #   out <- geodist
  #   message("Matrix of class 'units' returned. Distances are in km.")
  # }
  # if(return == "sf"){
  #   out <- geodistsf
  #   message("Class 'sf' returned. Distances are in km.")
  # }
  # if(return == "dist" | return == "distmat"){
  #   out <- as.dist(geodist)
  #   message("Distance matrix returned (default). Distances are in km.")
  # } else if(return == "matrix"){
  #   out <- as.matrix(geodist)
  #   message("Matrix returned. Distances are in km.")
  # } else if(return == "graph"){
  #   out <- reshape2::melt(as.matrix(geodist), na.rm = TRUE)
  #   colnames(out)[1] <- "lang1"
  #   colnames(out)[2] <- "lang2"
  #   colnames(out)[3] <- "dist"
  #   message("Graph returned")
  # }
  geodist <- as.dist(geodist)
return(geodist)
}


#' Distances between spatial points, as the bird flies.
#'
#' Calculate Euclidean distances between spatial points, as the bird flies (great circle distances/Haversine distances).
#'
#' @param points Spatial point object
#' @param label Column name or number indicating how objects should be labeled
#'
#' @return dist object with distances (km) between all points
#' @export
#' @family <geodist>
#'
#' @examples
#' pointdist_bird(points = points, label = "glottocode")
pointdist_bird <- function(points, label){
  if(is.null(label)){label <- "glottocode"}

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
#' @param label Column name or index to be used for labeling objects.
#' @family <geodist>
#' @return dist object with distances from each point (km) to the nearest line.
#' @export
#'
#' @examples
#' nearestline_bird(points = points, lines = rivers, label = "glottocode")
#' nearestline_bird(points = points, lines = roads, label = "glottocode")
nearestline_bird <- function(points, lines, label){
  points <- contransform_lonlat(points)
  lines <- contransform_lonlat(lines)
  nearest <- sf::st_nearest_feature(x = points, y = lines)
  geodist <- sf::st_distance(x = points, y = lines[nearest,], by_element = TRUE)
  geodist <- geodist %>% units::set_units(km)
  geodist <- round(geodist)
  # if(return == "sf"){
  #   points[ , "dist"] <- geodist
  #   geodistsf <- points
  # } else{names(geodist) <- points[, "name", drop = T]
  # }

  return(geodist)
}

#' Distances between spatial points, over land
#'
#' Least-cost distance between spatial points. Based on the Dijkstra (1959)
#' algorithm as implemented in the gdistance package (van Etten, 2017)
#'
#' @param points geoglot or sf object
#' @param label Column name or index to be used for labeling objects.
#' @param topography raster object or path to raster object
#'
#' @return dist object
#' @export
#' @family <geodist>
#' @examples
#' ppath <- "C:/Users/sjnor/surfdrive/Projecten en schrijfsels/Papers in progress/Isolates/output/nwa.gpkg"
#' topopath <- "D:/Global/Topography/SRTM/250m/South America/SRTM250mSA.tif"
#' points <- sf::st_read(ppath)
#' points <- sf::st_geometry(points)
#' points <- points[c(1:10),] # for testing only
#' pointdist_topo(points = points, topography = topopath)
pointdist_topo <- function(points, label, topography){
  # TODO: add units (km, hours, etc.)
  # https://stackoverflow.com/questions/36523709/r-gdistance-different-results-for-acccost-and-costdistance
  # https://gis.stackexchange.com/questions/244364/least-cost-path-barrier-r-gdistance
  if(is_raster(topography)){r <- topography
  } else {
    # TODO: check if path exists, and include try / tryCatch
    r <- raster::raster(topography, package = "gdistance") # assume it is a path
  }

  p <- sf::as_Spatial(points)

  if(!identicalcrs(x = r, y= p)) {stop("coordinate reference systems do not match.")}

  rc <- raster::crop(r, p) # added to reduce memory usage. TODO: see suggestions here: https://discuss.ropensci.org/t/how-to-avoid-space-hogging-raster-tempfiles/864



  # tr <- gdistance::transition(x = rc, transitionFunction = mean, directions = 8)
  # trc <- gdistance::geoCorrection(tr, type = "c")
  # # should be saved in temporary directory?
  # geodist <- gdistance::costDistance(x = trc, fromCoords = p, toCoords = p)
  # return(geodist)

  # # Example 1 from van Etten 2017
  # altDiff <- function(x) x[2] - x[1]
  # hd <- gdistance::transition(rc, altDiff, 8, symm= FALSE)
  # slope <- gdistance::geoCorrection(hd)
  # adj <- raster::adjacent(rc, cells = 1:raster::ncell(rc), pairs = TRUE, directions = 8)
  # speed <- slope
  # speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
  # conductance <- gdistance::geoCorrection(speed)
  #
  # AtoB <- gdistance::shortestPath(conductance, p[1,], p[2,], output = "SpatialLines")
  # BtoA <- gdistance::shortestPath(conductance, p[2,], p[1,], output = "SpatialLines")

  # Alternative
  geodist <- topoDistance::topoDist(DEM = rc, pts = p, directions = 8, paths = FALSE, zweight = 1)

}





#' Calculate mean distance from one feature to all other features
#'
#' @param dist \code{dist} object
#'
#' @return
#' @family <geodist><glottodist>
#' @export
#' @examples
#' meandist(dist)
meandist <- function(dist){
  distmat <- contransform_distmat(dist)
  totdist <- rowSums(distmat)
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
#' @family <geodist><glottodist>
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
#' @family <geodist><glottodist>
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
#' @family <geodist><glottodist>
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





