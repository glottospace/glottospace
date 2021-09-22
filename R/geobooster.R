
glot2geoglot <- function(glottodata, lon = "longitude", lat = "latitude"){
  if(class(glottodata)[1] == "sf"){stop("glottodata is already a spatial object")}
  glottolatlon <- glottodata %>%
    dplyr::filter(!is.na(!!as.symbol(lon))) %>%
    dplyr::filter(!is.na(!!as.symbol(lat)))

  glottodata <- sf::st_as_sf(x = as.data.frame(glottolatlon),
                             coords = c(lon, lat),
                             crs = 4326) #https://epsg.io/4326

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
#'
#' @examples
#' glottodata <- get_glottodata(meta = FALSE)
#' glottodata_addcountries(glottodata)
glottodata_addcountries <- function(glottodata){
  glottodata <- glottodata_makespatial(glottodata)

    # Adding names of countries and continents
    world <- rnaturalearth::ne_countries(returnclass = "sf", scale = "medium")
    world <- world[, c("name", "continent", "subregion")]
    names(world)[1] <- "country"
    names(world)[3] <- "region"

    world <- sf::st_make_valid(world)
    sf::st_join(x = glottodata, y = world, left = TRUE)
}

#' Make glottodata spatial
#'
#' Adds coordinates to glottodata.
#' If glottodata does not contain a geometry column, it will be added.
#'
#' @param glottodata User-provided glottodata
#' @aliases glottodata_addcoords
#' @return glottodata with a GEOMETRY column
#' @export
#'
#' @examples
#' glottodata <- get_glottodata()
#' glottodata_makespatial(glottodata)
glottodata_makespatial <- glottodata_addcoords <- function(glottodata){
  if(class(glottodata)[1] != "sf") {
    glottodata <- join_glottospace(glottodata)
  glottodata <- sf::st_sf(glottodata)
  }
  glottodata
}

