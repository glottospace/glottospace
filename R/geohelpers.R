# TODO: points2pols: Make more flexible country or continent, if user specifies country = "South America" this should also work.
# Or would it be possible to do that automatically: check unique values in country and continent.
# If there are many countries and one continent, mask by continent.
# If there is one or a few countries, mask by country


#' Mosaic raster tiles
#'
#' @param path path to folder containing multiple raster files
#' @param outfile file path where to save resulting raster mosaic
#' @param overwrite
#' @param na.rm
#' @param subdirs
#'
#' @return
#' @export
#'
#' @examples
mosaicras <- function(path = NULL, outfile = "mosaic.tif", overwrite = FALSE, na.rm = TRUE, subdirs = TRUE){
  if(xfun::file_ext(outfile) == ""){
    outfile <- paste0(outfile, ".tif")
  }

  tifls <- list.files(path = path, pattern = ".tif", recursive = subdirs)
  tifls <- paste(path, tifls, sep = "/")

  rasls <- lapply(tifls, raster::raster)

  rasls$fun <- mean
  names(rasls)[1:2] <- c('x', 'y')
  rasls$filename <- paste(path, outfile, sep = "/")
  rasls$na.rm <- na.rm
  rasls$overwrite <- overwrite

  message(paste0("Creating mosaic from ", length(tifls), " .tif files. Output (", outfile, ") stored in: ", path))
  do.call(raster::mosaic, rasls)
}

#' Merge multiple shapefiles
#'
#' @param paths
#' @param selection
#' @param outfile
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
mergevec <- function(paths = NULL, selection = NULL, outfile = NULL, overwrite = FALSE){
  layers <- lapply(paths, st_read)
  out <- sf::st_as_sf(data.table::rbindlist(layers))         # MUCH faster than: layers <- do.call(rbind, layers)
  message(paste("Loading and merging", length(paths), " files."))
  if(!is.null(selection)){out <- out[,selection]}
  if(!is.null(outfile)){gs_save(object = out, filename = outfile)}
  return(out)
}

#' Convert glottopoints to polygons
#'
#' @param glottopoints geoglot object (glottopoints)
#' @param interpolation Interpolation method, either "buffer" or "voronoi" (synonymous with "thiessen")
#' @param radius In case interpolation method "buffer", the radius in km.
#' @param country Optionally mask output by country boundaries
#' @param continent Optionally mask output by continent boundaries
#'
#' @return
#' @export
#'
#' @examples
#' gb <- glottologbooster(glottologdata = glottobase)
#' gbsa <- glottofilter(glottodata = gb, continent = "South America")
#'
#' pols <- points2pols(glottopoints = gbsa, interpolation = "thiessen", continent = "South America")
#' plot(pols[,"family_size"])
points2pols <- function(glottopoints, interpolation = "buffer", radius = NULL, country = NULL, continent = NULL){
  glottopoints <- contransform_lonlat(glottopoints)
  # Alternative could be to convert to equidistant projection: https://epsg.io/54032
  epsg_utm <- lonlat2utm(sf::st_coordinates(glottopoints))
  pts <- sf::st_transform(glottopoints, sf::st_crs(epsg_utm))
  if(interpolation == "buffer"){
    message(paste0('Creating buffer within a radius of ', radius, ' km.'))
    radius <- radius*1000 # convert km to meters because unit of st_buffer should be meters (crs is transformed to utm, in case lon/lat it would have been degrees.).
    pols <- sf::st_buffer(x = pts, dist = radius)
  }
  if(interpolation == "voronoi" | interpolation == "thiessen"){
    # Interpolate categorical data (e.g. family)
    # https://rspatial.org/raster/analysis/4-interpolation.html
    # https://r-spatial.github.io/sf/reference/geos_unary.html

    pols <- sf::st_collection_extract(sf::st_voronoi(do.call(c, sf::st_geometry(pts))))
    # st_crs(pols) <- st_crs(pts)
    pols <- sf::st_set_crs(x = pols, value = sf::st_crs(pts))
    # match them to glottopoints:
    pts$pols <- pols[unlist(sf::st_intersects(pts, pols))]
    pols <- sf::st_set_geometry(pts, "pols")
    if(!is.null(radius)){message("argument 'radius' not relevant for the specified interpolation.")}
  }

  if(!purrr::is_empty(country) | !purrr::is_empty(continent) ){
    country <- rnaturalearth::ne_countries(country = country, continent = continent, returnclass = "sf", scale = "medium")
    country <- sf::st_geometry(country)
    country <- sf::st_transform(country, sf::st_crs(epsg_utm))
    pols <- sf::st_intersection(pols, country) # crop to country boundaries
  } else if (!purrr::is_empty(country) & !purrr::is_empty(country)) {
          stop("Please supply either country or continent, noth both")
  }

  # Convert back to WGS84
  pols <- sf::st_transform(pols, crs = 4326)

  return(pols)
}

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

identicalcrs <- function(x, y){
  base::identical(sf::st_crs(x), sf::st_crs(y))
}

is_raster <- function(object){
  (class(object)[1] == "RasterLayer") | (class(object)[1] == "RasterStack")
}

is_sf <- function(object){
  class(object)[1] == "sf"
}

checkdata_crsmissing <- function(object){
  is.na(sf::st_crs(object))
}

is_point <- function(object){
  all(unique(sf::st_geometry_type(object)) == "POINT")
}

is_polygon <- function(object){
  any(unique(sf::st_geometry_type(object)) == "POLYGON")
}


