#' Get detailed info for a specific glottocode
#'
#' This function does three things for a glottocode:
#' - Plot its location on the Earth
#' - Open the associated webpage on www.glottolog.org
#' - Check whether it exists in glottolog
#'
#' @param glottocode
#'
#' @return
#' @export
#'
#' @examples
#' glottocode("yucu1253")
glottocode <- function(glottocode){
  if(length(glottocode) > 1){stop("Please provide a single glottocode")}
  if(glottocodes_exist(glottocode)){
    message("glottocode is valid")
    glottocode_location(glottocode)
    glottocode_online(glottocode)
  } else{
    message("glottocode not found")
  }
}


#' Show location of glottocode on globe
#'
#' @param glottocode
#'
#' @return
#' @export
#' @keywords internal
#' @examples
#' glottocode_location("yucu1253")
glottocode_location <- function(glottocode){

  language <- glottofilter(glottocode = glottocode)
  lon0 = st_coordinates(language)[1]
  lat0 = st_coordinates(language)[2]
  language <- as_s2_geography(paste0("POINT(", lon0, " ", lat0, ")") )

  earth = s2::as_s2_geography(TRUE)
  continents = s2::s2_data_countries()
  oceans = s2::s2_difference(earth, s2_union_agg(continents))
  b = s2::s2_buffer_cells(language, 9800000) # visible half
  i = s2::s2_intersection(b, oceans) # visible ocean
  continents = s2::s2_intersection(b, continents)
  plot(sf::st_transform(sf::st_as_sfc(i), paste0("+proj=ortho +lat_0=",lat0, " +lon_0=",lon0) ), col = 'lightblue')
  plot(sf::st_transform(sf::st_as_sfc(continents), paste0("+proj=ortho +lat_0=",lat0, " +lon_0=",lon0) ), col = "lightgrey", add = TRUE)
  plot(sf::st_transform(sf::st_as_sfc(language), paste0("+proj=ortho +lat_0=",lat0, " +lon_0=",lon0) ), col = "darkred", pch = 1, cex = 3, lwd = 2, add = TRUE)
}


#' Check whether glottocodes exist in glottolog
#'
#' @param glottocode A glottocode or character vector of glottocodes
#'
#' @return A logical vector
#' @export
#' @family <glottocheck><glottosearch>
#' @examples
#' glottocodes_exist(c("yucu1253"))
#' glottocodes_exist(c("yucu1253", "abcd1234"))
glottocodes_exist <- function(glottocode){
  glottosearch_exist(search = glottocode, columns = "glottocode")
}

#' Open url in web browser for glottocode
#'
#' @param glottocode
#'
#' @return
#' @export
#' @family <glottocheck><glottosearch>
#' @keywords internal
#' @examples
#' glottocode_online("yucu1253")
glottocode_online <- function(glottocode){
  url <- paste0("https://glottolog.org/resource/languoid/id/", glottocode)
  utils::browseURL(url)
}
