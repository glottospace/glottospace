
#' Make glottodata spatial
#'
#' This function takes glottodata (either with or without metadata) and turns it into spatial points or polygons.
#'
#' @param glottodata A glottodata table, or list of a glottodata table and metadata table(s)
#' @param method In case output should be polygons, interpolation method, either "buffer" or "voronoi" (synonymous with "thiessen")
#' @param radius In case interpolation method "buffer", the radius in km.
#' @param country Optionally mask output by country boundaries
#' @param continent Optionally mask output by continent boundaries
#'
#' @return
#' @export
#'
#' @examples
#' glottodata <- glottoget_path()
#' glottospacedata <- glottospace(glottodata)
#'
glottospace <- function(glottodata, method = NULL, radius = NULL, country = NULL, continent = NULL){
  if(!is_sf(glottodata)){
    #   if(glottocheck_hasmeta(glottodata) ){
    # glottometa <- glottodata[-1]
    # glottodata <- glottodata[[1]]
    # hasmeta <- TRUE
    #   }
    glottodata <- glottospace_addcoords(glottodata)
  }

  if(any( c(!is.null(method), !is.null(radius), !is.null(country), !is.null(continent) ) ) ){
    glottodata <- glottospace_points2pols(glottopoints = glottodata, method = method, radius = radius, country = country, continent = continent)
  }

  # if(hasmeta){
  #   glottodata <- glottojoin_meta(glottodata = glottodata, glottometa = glottometa)
  # }
return(glottodata)
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
#' @keywords internal
#' @examples
#' gb <- glottobooster(glottologdata = glottobase)
#' gbsa <- glottofilter(glottodata = gb, continent = "South America")
#'
#' pols <- glottospace_points2pols(glottopoints = gbsa, method = "thiessen", continent = "South America")
#' plot(pols[,"family_size"])
glottospace_points2pols <- function(glottopoints, method = NULL, radius = NULL, country = NULL, continent = NULL){
  if(is.null(method)){method <- "buffer"}
  # FIXME: area of buffers is not equal!
  # sf::st_area(glottodata)
  glottopoints <- contransform_lonlat(glottopoints)
  # Alternative could be to convert to equidistant projection: https://epsg.io/54032
  epsg_utm <- lonlat2utm(sf::st_coordinates(glottopoints))
  pts <- sf::st_transform(glottopoints, sf::st_crs(epsg_utm))
  if(method == "buffer"){
    message(paste0('Buffer created with a radius of ', radius, ' km.'))
    radius <- radius*1000 # convert km to meters because unit of st_buffer should be meters (crs is transformed to utm, in case lon/lat it would have been degrees.).
    pols <- sf::st_buffer(x = pts, dist = radius)

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
    pols <- suppressWarnings(sf::st_intersection(pols, unicountry) ) # crop to country boundaries
  } else if (!purrr::is_empty(country) & !purrr::is_empty(country)) {
    stop("Please supply either country or continent, noth both")
  }

  # Convert back to WGS84
  pols <- sf::st_transform(pols, crs = 4326)

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
#' @return
#' @export
#' @keywords internal
glottospace_coords2sf <- function(glottodata, lon = "longitude", lat = "latitude"){
  if(class(glottodata)[1] == "sf"){stop("glottodata is already a spatial object")}
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
#' @export
#' @keywords internal
#' @examples
#' glottodata <- glottoget_glottodata(meta = FALSE)
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
#' @export
#' @examples
#' glottodata <- glottoget("demodata")
#' glottospace_addcoords(glottodata)
glottospace_addcoords <- function(glottodata){
  if(class(glottodata)[1] != "sf") {
    glottodata <- glottojoin_space(glottodata)
    glottodata <- sf::st_sf(glottodata)
  }
  glottodata
}


