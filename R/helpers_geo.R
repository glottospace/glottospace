# TODO: points2pols: Make more flexible country or continent, if user specifies country = "South America" this should also work.
# Or would it be possible to do that automatically: check unique values in country and continent.
# If there are many countries and one continent, mask by continent.
# If there is one or a few countries, mask by country

#' Stack raster layers from local paths
#'
#' @param filepaths Character vector with paths to .tif files
#' @param newnames Optional vector with new names to be assigned to each layer
#' @param extent Optional \code{extent} object to crop
#' @param outfile Optional filename to store resulting rasterstack (.grd is preferred format because .tif doesn't preserve layernames). This might take a while, depending on the file size)
#'
#' @return
#' @keywords internal
#' @export
#'
#' @examples
#' dir <- "D:/Global/Climate/WORLDCLIM2/1km/South America"
#' files <- list.files(dir, full.names = TRUE)
#' newnames <- paste0("bio", sprintf("%02d", seq(1,19)))
rasterstack <- function(filepaths, outfile, newnames = NULL, extent = NULL){
  stack <- raster::stack(filepaths)
  if(!is.null(newnames)){
    if(length(filepaths) != length(newnames)){stop("filepaths and newnames should have the same length")}
    names(stack) <- newnames
  }

  if(!is.null(extent)){
    stack <- raster::crop(stack, extent)
  }

  if(!is.null(outfile)){
    message(paste0("saving to ", outfile))
    if(tools::file_ext(outfile) == ".grd"){
      raster::writeRaster(stack, filename = outfile, format = "raster", overwrite = TRUE)
    }
    if(tools::file_ext(outfile) == ".tif"){
      raster::writeRaster(stack, filename = outfile, format = "GTiff", overwrite = TRUE)
    }

  }
  stack
}

#' Mosaic raster tiles
#'
#' @param path path to folder containing multiple raster files
#' @param outfile file path where to save resulting raster mosaic
#' @param overwrite
#' @param na.rm
#' @param subdirs
#'
#' @return
#' @keywords internal
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
#' @keywords internal
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
#' @param method Interpolation method, either "buffer" or "voronoi" (synonymous with "thiessen")
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
#' pols <- points2pols(glottopoints = gbsa, method = "thiessen", continent = "South America")
#' plot(pols[,"family_size"])
points2pols <- function(glottopoints, method = "buffer", radius = NULL, country = NULL, continent = NULL){
  # FIXME: area of buffers is not equal!
  # sf::st_area(glottodata)
  glottopoints <- contransform_lonlat(glottopoints)
  # Alternative could be to convert to equidistant projection: https://epsg.io/54032
  epsg_utm <- lonlat2utm(sf::st_coordinates(glottopoints))
  pts <- sf::st_transform(glottopoints, sf::st_crs(epsg_utm))
  if(method == "buffer"){
    radius <- radius*1000 # convert km to meters because unit of st_buffer should be meters (crs is transformed to utm, in case lon/lat it would have been degrees.).
    pols <- sf::st_buffer(x = pts, dist = radius)
    message(paste0('Buffer created with a radius of ', radius, ' km.'))
  }
  if(method == "voronoi" | method == "thiessen"){
    # Interpolate categorical data (e.g. family)
    # https://rspatial.org/raster/analysis/4-interpolation.html
    # https://r-spatial.github.io/sf/reference/geos_unary.html

    pols <- sf::st_collection_extract(sf::st_voronoi(do.call(c, sf::st_geometry(pts))))
    # st_crs(pols) <- st_crs(pts)
    pols <- sf::st_set_crs(x = pols, value = sf::st_crs(pts))
    # match them to glottopoints:
    pts$pols <- pols[unlist(sf::st_intersects(pts, pols))]
    pts$points <- pts$geometry # these lines are redundant because I could just set the active geometry to the polygons, but for the user this seems more intuitive
    pols <- sf::st_drop_geometry(pts) %>% dplyr::relocate(pols, .after = last_col()) %>% dplyr::rename(geometry = pols)
    pols <- sf::st_set_geometry(pols, "geometry")
    if(!is.null(radius)){message("argument 'radius' not relevant for the specified interpolation method.")}
  }

  if(!purrr::is_empty(country) | !purrr::is_empty(continent) ){
    country <- rnaturalearth::ne_countries(country = country, continent = continent, returnclass = "sf", scale = "medium")
    country <- sf::st_geometry(country)
    country <- sf::st_transform(country, sf::st_crs(epsg_utm))
    # merge polygons
    unicountry <- sf::st_union(country)
    unicountry <- sfheaders::sf_remove_holes(unicountry)
    pols <- sf::st_intersection(pols, unicountry) # crop to country boundaries
  } else if (!purrr::is_empty(country) & !purrr::is_empty(country)) {
          stop("Please supply either country or continent, noth both")
  }

  # Convert back to WGS84
  pols <- sf::st_transform(pols, crs = 4326)

  return(pols)
}


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
  if(!sf::st_is_longlat(data)) {
    # raster::isLonLat
    data <- sf::st_transform(x = data, crs = "EPSG:4326")
  }
  return(data)
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
  as.matrix(dist)
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
checkdata_crsmissing <- function(object){
  is.na(sf::st_crs(object))
}


#' Check whether the geometry of a spatial object is POINT
#'
#' @param object any object
#'
#' @return TRUE/FALSE
#' @noRd
#'
is_point <- function(object){
  all(unique(sf::st_geometry_type(object)) == "POINT")
}


#' Check whether the geometry of a spatial object is POLYGON
#'
#' @param object any object
#'
#' @return TRUE/FALSE
#' @noRd
#'
is_polygon <- function(object){
  any(unique(sf::st_geometry_type(object)) == "POLYGON")
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
