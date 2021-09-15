#' Extract and add environmental covariates
#'
#' @param glottodata Spatial vector with glottopoints or glottopols
#' @param geodata Spatial data in raster (RasterLayer or RasterStack) or vector format, or a path to such a file.
#' @param radius Radius (km) from which raster values should be extracted.
#' @param fun One of "mean", "median", "min", "max", "sd", "modal". Alternatively, a user-defined function can be specified, as in fun = function(x){mean(x,na.rm=T)}
#' @param add By default, extracted values are added to glottodata.
#' @param funnames By default, the name of the function is added to the new variable. For example: "elevation_mean". Use funnames = FALSE if you want to keep the original names
#'
#' @return
#' @export
#' @aliases addgeodata
#'
#' @examples
#' path <- "D:/Global/Societal/travel time/travel_time_to_cities_1.tif"
#' geodata <- get_geodata(path)
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
    geodata <- get_geodata(path = geodata)
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
#'
#' @return
#' @export
#'
#' @examples
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
    stop("glottodata is not recognized as a spatial object, use 'glot2geoglot' to convert it")
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
