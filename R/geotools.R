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
#' @family <geodata><geotools>
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
#' @param outfile file path where to save resulting raster mosaic. Output will be save as .tif
#' @param overwrite Default is FALSE
#' @param na.rm Default is TRUE
#' @param subdirs By default, raster files in subdirectories are also included in the mosaic.
#' @family <geotools><geodata>
#' @return
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{mosaicras(path = "path/localpath", outfile = "mosaicv2.tif")}
mosaicras <- function(path, outfile = "mosaic.tif", overwrite = FALSE, na.rm = TRUE, subdirs = TRUE){
  if(tools::file_ext(filename) == ""){
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
#' @param paths Character vector of paths to shapefile
#' @param selection Optionally select data from merged shapefiles
#' @param outfile Optionally save file locally
#' @param overwrite Whether outfile should be overwritten if it exists
#' @family <geodata><geotools>
#' @return
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{mergevec(paths = c("vectora.shp", "vectorb.shp"))}
mergevec <- function(paths, selection = NULL, outfile = NULL){
  layers <- lapply(paths, st_read)
  out <- sf::st_as_sf(data.table::rbindlist(layers))         # MUCH faster than: layers <- do.call(rbind, layers)
  message(paste("Loading and merging", length(paths), " files."))
  if(!is.null(selection)){out <- out[,selection]}
  if(!is.null(outfile)){glottosave(glottodata = out, filename = outfile)}
  return(out)
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
