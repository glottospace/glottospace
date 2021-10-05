#' Extract and add environmental covariates
#'
#' @param glottodata Spatial vector with glottopoints or glottopols
#' @param geodata Spatial data in raster (RasterLayer or RasterStack) or vector format, or a path to such a file.
#' @param radius Radius (km) from which raster values should be extracted.
#' @param fun One of "mean", "median", "min", "max", "sd", "modal". Alternatively, a user-defined function can be specified, as in fun = function(x){mean(x,na.rm=T)}
#' @param add By default, extracted values are added to glottodata.
#' @param funnames By default, the name of the function is added to the new variable. For example: "elevation_mean". Use funnames = FALSE if you want to keep the original names
#' @family <geodata><geotools>
#' @return
#' @export
#' @aliases addgeodata
#'
#' @examples
#' path <- "D:/Global/Societal/travel time/travel_time_to_cities_1.tif"
#' geodata <- geoget(path)
#'
#' # extract from points
#' glottopoints <- glottofilter(continent = "South America")
#' glottopoints <- glottopoints[1:3, ]
#' extractgeodata(glottodata = glottopoints, geodata = geodata, radius = 10, fun = "mean")
#'
#' # extract from polygons
#' glottopoints <- glottofilter(country = "Netherlands")
#' glottopols <- points2pols(glottopoints, method = "voronoi", country = "Netherlands")
#' extractgeodata(glottodata = glottodata, geodata = geodata,fun = "mean")
extractgeodata <- function(glottodata, geodata, radius = NULL, fun = NULL, add = TRUE, funnames = TRUE){

  if(is.character(geodata)){
    geodata <- geoget(path = geodata)
  } else if(is.object(geodata)){
    geodata <- geodata
  }

  if(!is.null(fun)){
    if(funnames == TRUE){
      if(is.function(fun)){funname <- "expr"
      } else {funname <- fun}
      names(geodata) <- paste(names(geodata),funname, sep = "_")
    }
    fun <- eval(as.symbol(fun))
  }

  if(class(geodata)[1] == "RasterLayer" | class(geodata)[1] == "RasterStack"){
    out <- extractraster(glottodata = glottodata, geodata = geodata, radius = radius, fun = fun, add = add)
  }

  if(class(geodata)[1] == "sf"){
    out <- extractvec(geodata = geodata, ...)
  }

  return(out)
}

#' Extract raster values from glottopoints or glottopols
#'
#' Raster values can be extracted at glottopoint locations, from a search radius surrounding glottopoints, or from glottopols
#'
#' @param glottodata Spatial vector with glottopoints or glottopols
#' @param geodata Raster
#' @param radius Radius (km) from which raster values should be extracted.
#' @param fun One of mean, median, min, max, sd, modal. Alternatively, a user-defined function can be specified, as in fun = function(x){mean(x,na.rm=T)}
#' @param add By default, extracted values are added to glottodata.
#' @keywords internal
#' @family <geodata><geotools>
#' @return
#' @export
#'
extractraster <- function(glottodata, geodata, radius = NULL, fun = NULL, add = TRUE){
  # exact_extract(ras, poly, 'mean')


  if(is_point(glottodata) ){

    if(!is.null(radius)){
      geodata <- contransform_lonlat(geodata)
      message(paste0('Extracting values within a radius of ', radius, ' km.'))
      radius <- radius*1000 # convert km to meters because unit of raster::extract should be meters if data are not projected (lat/lon).
      if(is.null(fun)){
        stop('Please indicate how you want to summarize the values within the specified radius.')
      }
    }

    extracted <- raster::extract(x = geodata, y = glottodata, buffer = radius, fun = fun, na.rm = TRUE)
    if(add == T){
      extracted <- as.data.frame(extracted)
      names(extracted) <- names(geodata)
      glottodata <- cbind(glottodata, extracted)
    } else{
      glottodata <- extracted
    }


    message("geodata extracted")
    return(glottodata)
  }

  if(is_polygon(glottodata)){
    if(is.null(fun)){
      stop('Please indicate how you want to summarize the values within each polygon.')
    }

    extracted <- raster::extract(x = geodata, y = glottodata, fun = fun, na.rm = TRUE)
    if(add == T){
      extracted <- as.data.frame(extracted)
      names(extracted) <- names(geodata)
      glottodata <- cbind(glottodata, extracted)
    } else{
      glottodata <- extracted
    }
    message("geodata extracted")
    # FIXME: After extraction, active geometry is set to points, this should remain polygons.
    glottodata <- st_set_geometry(glottodata, "geometry")
    return(glottodata)
  }

}

extractvector <- function(glottodata, geodata, radius = NULL, fun = NULL, add = T){
  geodata <- contransform_lonlat(geodata)

  if(is_point(glottodata) & is.null(radius)){
    extracted <- sf::st_intersection(x = glottodata, y = geodata)
  } else if(is_point(glottodata) & !is.null(radius)){

    # alternative approach: st_join(x = points, y = rivers, join = st_is_within_distance, dist = 10)
    # convert to equidistant world projection: https://epsg.io/54032
    glottodata  <- points2pols(glottopoints = glottodata, method = "buffer", radius = radius)
  } else if(is_polygon(glottodata)){
    glottodata <- glottodata
  } else {
    stop("glottodata is not recognized as a spatial object, use 'glottodata_makespatial' to convert it")
  }

  if(is.null(fun)){
    stop('Please indicate how you want the extracted values to be summarized.')
  }

  if(fun == "totlength"){
    extracted <- extractvector_totlength(glottopols = glottodata, geodata = geodata)
  }

  # if(fun == "proportion"){
  # if glottopols: proportional intersection with other polygons.
  #   # ecoregion proportion
  #   # # https://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r
  # }

  if(add == T){
    glottodata <- extracted # optional TO ADD name_fun
  } else{
    glottodata <- sf::st_drop_geometry(extracted)[,names(sf::st_drop_geometry(geodata))]
  }

  message("Data extracted")
  return(glottodata)

}

extractvector_totlength <- function(glottopols, geodata, id = NULL){

  if(length(unique(sf::st_area(glottodata))) != 1){warning("Not all glottopols have the same area. fun='totlength' only meaningful if all glottopols have same area. You might consider dividing by total area.")}

  if(is.null(id)){id <- "glottocode"}
  int <- sf::st_intersects(x = glottopols, y = geodata)
  int <- lengths(int) > 0
  extracted <- sf::st_intersection(x = glottopols, y = geodata)
  extracted$len <- sf::st_length(extracted)
  extracted <- extracted %>% dplyr::group_by(id) %>% dplyr::summarise(totlength = sum(len))
  extracted <- sf::st_drop_geometry(extracted)
  extracted$totlength <- extracted$totlength %>% units::set_units(km)
  extracted <- round(extracted$totlength)

  tmpvec <- rep(NA, nrow(glottopols))
  tmpvec <- ifelse(int, extracted, 0)
  extracted <- tmpvec
}


# if(name == "riverras"){
#   # TO DO: reclass data (using raster::reclassify), different possibilities. For example reclass all pixels to either water or land (to calculate number of water pixels in buffer, not accurate if riverwidth is smaller than cell size). Or keep only centerlines (to calculate sum of river width in focal area, although then line features might be better).
#
#   if(res == "90m" & region == "South America"){file <- "merit_width_SA_90m.tif"} else {file <- ""}
#   path <- gs_geopath(pathgroup = "Hydrography/MERIT Hydro/river width", res = res, region = region, file = file)
#   ras <- raster::raster(path)
#   cat(paste("Loading raster:", name, "at", res, "resolution for:", region))
#   if(!is.null(ras)){
#     names(ras) <- name
#     message(paste0("River width loaded (", file, ") for region:", region))
#     out <- ras
#     cat(paste("The values larger than 0 represents the river width at the channel centerlines.
#       The value -1 represents non-centerline water pixels, and the value 0 corresponds to the non-water pixels.
# The undefined pixels (oceans) are represented by the value -9999."))


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
