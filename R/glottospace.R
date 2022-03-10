
#' Make glottodata spatial and generate language polygons from points.
#'
#' This function takes glottodata (either with or without metadata) and turns it into spatial points or polygons.
#'
#' @param glottodata A glottodata table, or list of a glottodata table and metadata table(s)
#' @param method Interpolation method, either "buffer" or "voronoi" (synonymous with "thiessen")
#' @param radius In case interpolation method "buffer", the radius in km.
#'
#' @return A spatial version of glottodata. In case glottodata has metadata, only glottodata
#'  will be converted to spatial (but all metadata tables are kept).
#'  Object returned as sf object, or a list of which the first element is an sf object, depending on the input.
#' @export
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' \donttest{
#' glottospacedata <- glottospace(glottodata, method = "voronoi")
#' }
glottospace <- function(glottodata, method = NULL, radius = NULL){

  stopifnot(glottocheck_isglottodata(glottodata))
  splitted <- glottosplitmergemeta(glottodata)
  glottodata <- splitted[[1]]

  if(!is_sf(glottodata)){
    glottodata <- glottospace_addcoords(glottodata)
  }

  if(!is.null(method)){
  if(method == "thiessen" | method == "voronoi"){
    glottodata <- glottospace_thiessen(glottodata = glottodata)
  }

  if(method == "buffer"){
    glottodata <- glottospace_buffer(glottodata = glottodata, radius = radius)
  }
  }

  glottodata <- glottosplitmergemeta(glottodata = glottodata, splitted = splitted)

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
#' @noRd
#'
#' @examples
#' glottodata <- glottofilter(country = "Netherlands")
#' glottodata <- glottofilter(continent = "South America")
glottospace_thiessen <- function(glottodata){
  crs_original <- sf::st_crs(glottodata)
  pts <- sf::st_transform(glottodata, sf::st_crs("ESRI:54032")) # convert to equidistant projection: https://epsg.io/54032

  pols <- sf::st_collection_extract(sf::st_voronoi(do.call(c, sf::st_geometry(pts))))
  pols <- sf::st_set_crs(x = pols, value = sf::st_crs(pts))

  # match glottopols to glottopoints:
  pts$pols <- pols[unlist(sf::st_intersects(pts, pols))]
  pols <- sf::st_drop_geometry(pts) %>% dplyr::relocate(pols, .after = dplyr::last_col()) %>% dplyr::rename(geometry = pols)
  pols <- sf::st_set_geometry(pols, "geometry")

  # Select boundaries
  countries <- unique(pols$country)
  countrypols <- rnaturalearth::ne_countries(country = countries, returnclass = "sf", scale = "medium")
  continents <- unique(countrypols$continent)
  continentpols <- rnaturalearth::ne_countries(continent = continents, returnclass = "sf", scale = "medium")

  # Threshold above which Thiessen polygons should be cropped to continental boundaries (instead of countries).
  if(sum(countrypols$name %in% continentpols$name) / length(continentpols$name) > 0.8){
    boundarypols <- continentpols
  } else {
    boundarypols <- countrypols
  }

  boundarypols <- sf::st_transform(boundarypols, crs = sf::st_crs("ESRI:54032") ) %>% sf::st_geometry()

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
  world <- rnaturalearth::ne_countries(returnclass = "sf", scale = "medium")
  world <- world[, c("admin", "continent", "sovereignt")]
  names(world)[1] <- "country"
  names(world)[3] <- "sovereignty"

  world <- sf::st_make_valid(world)
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


