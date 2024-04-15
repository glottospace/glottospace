
#' Make glottodata spatial and generate language polygons from points.
#'
#' This function takes glottodata (either with or without metadata) and turns it into spatial points or polygons.
#'
#' @param glottodata A glottodata table, or list of a glottodata table and metadata table(s)
#' @param method Interpolation method, either "buffer" or "voronoi" (synonymous with "thiessen")
#' @param radius In case interpolation method "buffer", the radius in km around the points. If method "thiessen", a buffer will be created into the ocean, particularly relevant for island languages.
#'
#' @return A spatial version of glottodata. In case glottodata has metadata, only glottodata
#'  will be converted to spatial (but all metadata tables are kept).
#'  Object returned as sf object, or a list of which the first element is an sf object, depending on the input.
#' @export
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' \donttest{
#' glottopols <- glottospace(glottodata, method = "voronoi")
#' }
#'
#' glottodata <- glottofilter(country = "Netherlands")
#' glottopols <- glottospace(glottodata, method = "buffer", radius = 20)
#' glottomap(glottopols)
#'
#' glottodata <- glottofilter(continent = "South America")
#' glottopols <- glottospace(glottodata, method = "thiessen")
#' glottomap(glottopols)
#'
#' glottodata <- glottofilter(country = "Philippines")
#' glottopols <- glottospace(glottodata, radius = 100, method = "thiessen")
#' glottomap(glottopols)
#'
glottospace <- function(glottodata, method = NULL, radius = NULL){

  if(glottocheck_isglottosubdata(glottodata)){stop("A spatial object can only be created from glottodata, not from glottosubdata.")}
  glottodata <- glottosimplify(glottodata)

  if(!is_sf(glottodata)){
    glottodata <- glottospace_addcoords(glottodata)
  }

  if(all(c("country", "continent") %nin% names(glottodata))){
    glottodata <- glottospace_addcountries(glottodata)
  }

  if(!is.null(method)){
    method <- tolower(method)
    if(method %nin% c("thiessen", "voronoi", "buffer")){stop("Method should be either 'buffer' or 'voronoi' (synonymous with 'thiessen') ")}
  if(method == "thiessen" | method == "voronoi"){
    glottodata <- glottospace_thiessen(glottodata = glottodata, radius = radius)
  }

  if(method == "buffer"){
    glottodata <- glottospace_buffer(glottodata = glottodata, radius = radius)
  }
  }

  # glottodata <- contrans_glottoclass(glottodata)

return(glottodata)
}

#' Create buffer around glottopoints
#'
#' @param glottodata spatial glottodata without metadata
#' @param radius radius in kilometers
#' @noRd
#'
#' @examples
#' glottodata <- glottofilter(country = "Netherlands")
glottospace_buffer <- function(glottodata, radius){
  if(is.null(radius)){stop("Please specify buffer radius")}
  crs_original <- sf::st_crs(glottodata)

  pts <- sf::st_transform(glottodata, sf::st_crs("ESRI:54032")) # convert to equidistant projection: https://epsg.io/54032
  message(paste0('Buffer created with a radius of ', radius, ' km.'))
  r <- radius*1000 # convert km to meters because unit of st_buffer should be meters (crs is transformed to equidistant with units = meters, in case lon/lat it would have been degrees.).
  pols <- sf::st_buffer(x = pts, dist = r)

  # Convert back to original crs
  pols <- sf::st_transform(pols, crs_original)

  return(pols)
}

#' Create Thiessen polygons
#'
#' @param glottodata spatial glottodata without metadata
#' @param radius radius in km
#'
#' @noRd
#'
#' @examples
#' glottodata <- glottofilter(country = "Netherlands")
#' glottopols <- glottospace_thiessen(glottodata)
#' glottomap(glottopols)
#'
#' glottodata <- glottofilter(continent = "South America")
#' glottopols <- glottospace_thiessen(glottodata)
#' glottomap(glottopols)
#'
#' glottodata <- glottofilter(country = "Philippines")
#' glottopols <- glottospace_thiessen(glottodata, radius = 100)
#' glottomap(glottopols)
#'
glottospace_thiessen <- function(glottodata, radius = NULL){
  crs_original <- sf::st_crs(glottodata)
  pts <- sf::st_transform(glottodata, sf::st_crs("ESRI:54032")) # convert to World Azimuthal Equidistant projection: https://epsg.io/54032

  pols <- sf::st_collection_extract(sf::st_voronoi(do.call(c, sf::st_geometry(pts))))
  pols <- sf::st_set_crs(x = pols, value = sf::st_crs(pts))

  # match glottopols to glottopoints:
  pts$pols <- pols[unlist(sf::st_intersects(pts, pols))]
  pols <- sf::st_drop_geometry(pts) %>% dplyr::relocate(pols, .after = dplyr::last_col()) %>% dplyr::rename(geometry = pols)
  pols <- sf::st_set_geometry(pols, "geometry")

  # Select boundaries
  geounits <- unique(pols$geounit)
  worldpol <- glottospace::worldpol
  geounitpols <- worldpol[worldpol$geounit %in% geounits,]
  continents <- unique(geounitpols$continent)
  continentpols <- worldpol[worldpol$continent %in% continents,]

  # Threshold above which Thiessen polygons should be cropped to continental boundaries (instead of countries).
  if(sum(geounitpols$geounit %in% continentpols$geounit) / length(continentpols$geounit) > 0.8){
    boundarypols <- continentpols
  } else {
    boundarypols <- geounitpols
  }

  boundarypols <- sf::st_transform(boundarypols, crs = sf::st_crs("ESRI:54032") ) %>% sf::st_geometry()

  if(!is.null(radius)){
  r <- radius*1000 # convert km to meters because unit of st_buffer should be meters (crs is transformed to equidistant with units = meters, in case lon/lat it would have been degrees.).
  boundarypols <- sf::st_buffer(x = boundarypols, dist = r)
  }

  # merge polygons
  boundary <- sf::st_union(boundarypols) %>% sf::st_make_valid()
  pols <- suppressWarnings(sf::st_intersection(pols, boundary) ) # crop to boundaries

  # Convert back to original crs
  pols <- sf::st_transform(pols, crs_original)
  pols <- sf::st_wrap_dateline(pols, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE)
  pols <- pols %>% sf::st_make_valid()

  return(pols)
}

#' Convert glottodata with lat/lon columns to simple feature class
#'
#' This function is used by glottobooster().
#'
#' @param glottodata glottodata table
#' @param lon Column name containing longitude
#' @param lat Column name containing latitude
#'
#'
#' @noRd
glottospace_coords2sf <- function(glottodata, lon = "longitude", lat = "latitude"){
  if(inherits(glottodata, what = "sf" )){stop("glottodata is already a spatial object")}
  glottolatlon <- glottodata %>%
    dplyr::filter(!is.na(!!as.symbol(lon))) %>%
    dplyr::filter(!is.na(!!as.symbol(lat)))

  glottodata <- sf::st_as_sf(x = as.data.frame(glottolatlon),
                             coords = c(lon, lat),
                             crs = 4326) #https://epsg.io/4326

return(glottodata)
}

#' Add names of countries, continents, and regions to glottodata
#'
#' Spatial intersection between glottodata and
#' \href{https://www.naturalearthdata.com/}{naturalearthdata} and extracts names
#' of countries, continents, and regions.
#'
#' @param glottodata User-provided glottodata
#' @aliases glottodata_addcontinents
#' @aliases glottodata_addregions
#'
#' @return A spatial (sf) object
#' @noRd
#' @examples
#' glottodata <- glottoget("demodata")
#' glottospace_addcountries(glottodata)
glottospace_addcountries <- function(glottodata){
  glottodata <- glottospace_addcoords(glottodata)

  # Adding names of countries and continents
  world <- glottospace::worldpol
  sf::st_join(x = glottodata, y = world, left = TRUE)
}

#' Make glottodata spatial
#'
#' Adds coordinates from glottolog to user-provided glottodata.
#' If glottodata does not contain a geometry column, it will be added.
#'
#' @param glottodata User-provided glottodata
#' @return glottodata with a GEOMETRY column
#' @noRd
#' @examples
#' glottodata <- glottoget("demodata")
#' glottospace_addcoords(glottodata)
glottospace_addcoords <- function(glottodata){
  if(!inherits(glottodata, what = "sf" )) {
    glottodata <- glottojoin_space(glottodata)
    glottodata <- sf::st_sf(glottodata)
  }
  glottodata
}



# glotto_param_sf <- function(glottodata, param,  longitude = NULL, latitude = NULL){
#   if (is(glottodata, "data.frame")){
#     if (!is.null(latitude) && latitude %nin% colnames(glottodata)){
#       stop(paste(paste(paste("The parameter \"", latitude, sep=""), "\"", sep=""), "is not founded.", sep = " " ))}
#
#     if (!is.null(longitude) && longitude %nin% colnames(glottodata)){
#       stop(paste(paste(paste("The parameter \"", longitude, sep=""), "\"", sep=""), "is not founded.", sep = " " ))}
#
#     if (
#       is.null(latitude) && is.null(longitude) &&
#       "longitude" %in% tolower(colnames(glottodata)) && "latitude" %in% tolower(colnames(glottodata))
#     ){longitude <- "longitude"
#       latitude <- "latitude"} else {
#         stop("Please specify the longitude and latitude variables.")
#       }
#
#     if (param %in% colnames(glottodata)){
#       param_table <- table(glottodata[, param])
#       geometry <- param_table |>
#         names() |>
#         lapply(
#           FUN = function(x){
#             glottodata[which(grambank[, param] == x), c(longitude, latitude)] |>
#               as.matrix() |>
#               sf::st_multipoint()}) |>
#         sf::st_as_sfc(crs = 4326)
#
#       param_val <- as.matrix(names(param_table))
#       colnames(param_val) <- param
#
#       param_sf <- sf::st_sf(param_val, geometry)
#
#     }
#     else{
#       stop(paste(paste(paste(paste("The dataset ", substitute(glottodata)), " does not contain the parameter "), param, sep=""), ".", sep=""))
#       # stop(paste(paste("The glottodata does not have the parameter", param, sep = " "), ".", sep=""))}
#     }
#
#     }
#   return(param_sf)
# }
#
#
#
#
#
#
#
#
#
#
#




