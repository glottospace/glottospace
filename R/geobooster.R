
glot2geoglot <- function(glottodata, lon = "longitude", lat = "latitude"){
  if(is_sf(glottodata)){stop("glottodata is already a spatial object")}
  glottolatlon <- glottodata %>%
    dplyr::filter(!is.na(!!as.symbol(lon))) %>%
    dplyr::filter(!is.na(!!as.symbol(lat)))

  glottodata <- sf::st_as_sf(x = as.data.frame(glottolatlon),
                             coords = c(lon, lat),
                             crs = 4326) #https://epsg.io/4326

}

#' Add names of countries, continents, and regions to glottodata
#'
#' @param glottodata
#' @aliases glottodata_addcontinents
#' @aliases glottodata_addregions
#'
#' @return
#' @export
#'
#' @examples
glottodata_addcountries <- function(glottodata){


    # Adding names of countries and continents
    world <- rnaturalearth::ne_countries(returnclass = "sf", scale = "medium")
    world <- world[, c("name", "continent", "subregion")]
    names(world)[1] <- "country"
    names(world)[3] <- "region"

    sf::st_join(x = glottodata, y = world, left = TRUE)


}


