#' Extract and add environmental covariates
#'
#' @param glottodata Spatial vector with glottopoints or glottopols
#' @param geodata Spatial data in raster or vector format, or a path to such a file.
#' @param radius Radius (km) from which raster values should be extracted.
#' @param fun One of mean, median, min, max, sd, modal. Alternatively, a user-defined function can be specified, as in fun = function(x){mean(x,na.rm=T)}
#' @param add By default, extracted values are added to glottodata.
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
#' extractgeodata(glottodata = glottopoints, geodata = geodata, radius = 10, fun = mean)
#'
#' # extract from polygons
#' glottopoints <- glottofilter(country = "Netherlands")
#' glottopols <- points2pols(glottopoints, interpolation = "voronoi", country = "Netherlands")
#' extractgeodata(glottodata = glottopols, geodata = geodata,fun = mean)
extractgeodata <- function(glottodata, geodata, radius = NULL, fun = NULL, add = TRUE){

  if(is.character(geodata)){
    geodata <- get_geodata(path = geodata)
  } else if(is.object(geodata)){
    geodata <- geodata
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

  if(is_point(glottodata) ){

  if(!is.null(radius)){
    geodata <- contransform_lonlat(geodata)
    message(paste0('Extracting values within a radius of ', radius, ' km.'))
    radius <- radius*1000 # convert km to meters because unit of raster::extract should be meters if data are not projected (lat/lon).
    if(is.null(fun)){
      stop('Please indicate how you want to summarize the values within the specified radius.')
    }
  }

    extracted <- raster::extract(x = geodata, y = glottodata, buffer = radius, fun = fun)
    if(add == T){
      glottodata[,names(geodata)] <- extracted # optional TO ADD name_fun
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

    extracted <- raster::extract(x = geodata, y = glottodata, fun = fun)
    if(add == T){
      glottodata[,names(geodata)] <- extracted # optional TO ADD name_fun
    } else{
      glottodata <- extracted
    }
    message("geodata extracted")
    return(glottodata)
  }

}

extractvector <- function(points = NULL, glottopols = NULL, geodata = NULL, add = T, radius = NULL, fun = NULL, ...){

  if(!is_empty(points) & is.null(radius)){
    extracted <- sf::st_intersection(x = points, y = geodata)
    if(add == T){
      # Default is to add covariates to points after extraction
      points <- extracted # optional TO ADD name_fun
    } else{
      points <- extracted[,names(st_drop_geometry(geodata))]
    }
  }

  if(!is_empty(points) & !is.null(radius)){

    # To add: check if geodata are in lat/lon (i.e. not projected), otherwise throw error/warning.
    if(is.null(fun)){
      stop('Please indicate how you want to summarize the values within the specified radius.
    Currently, only length is implemented for vector geodata ')
    }

    # alternative approach: st_join(x = points, y = rivers, join = st_is_within_distance, dist = 10)
    # convert to equidistant world projection: https://epsg.io/54032
    glottopols  <- points2pols(points = points, type = "buffer", radius = radius)

    if(fun == "totlength"){
      int <- sf::st_intersects(x = glottopols, y = geodata)
      int <- lengths(int) > 0
      extracted <- sf::st_intersection(x = glottopols, y = geodata)
      extracted$len <- st_length(extracted)
      extracted <- extracted %>% group_by(glottocode) %>% summarise(totlength = sum(len))
      extracted <- st_drop_geometry(extracted)
      extracted$totlength <- extracted$totlength %>% units::set_units(km)
      extracted <- round(extracted$totlength)

      tmpvec <- rep(NA, nrow(glottopols))
      tmpvec <- ifelse(int, extracted, 0)
      extracted <- tmpvec

    }
    if(add == T){
      # Default is to add covariates to points after extraction
      points$totlength <- extracted # optional TO ADD name_fun
    } else{
      points <- extracted
    }
  }


  # ecoregion proportion
  # # https://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r

  #   # To be implemented: If polygons are provided
  # if(!is_empty(glottopols)){
  # inter <- st_intersection(x = glottopols, y = data) # https://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r
  # }

  # To be implemented: if radius argument is provided, create buffer.
  # if(!is.null(radius)){
  #   # To add: check if data are in lat/lon (i.e. not projected), otherwise throw error/warning.
  #   message(paste0('Extracting Values within a radius of ', radius, ' km.'))
  #   buffer <- gs_polygonize(points = points, type = "buffer", radius = radius)
  # #   if(is.null(fun)){
  # #   #   stop('Please indicate how you want to summarize the values within the specified radius.
  # #   # Provide an argument to fun, as in fun = mean (e.g. median, min, max, sd, modal).
  # #   #      Or specify user-defined function, as in fun = function(x){mean(x,na.rm=T)} ')
  # #   }
  # }

  #
  message("
  Data extracted")
  return(points)

}

