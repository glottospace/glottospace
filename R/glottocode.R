#' Get detailed info for a specific glottocode
#'
#' This function does three things for a glottocode:
#' - Plot its location on the Earth
#' - Open the associated webpage on www.glottolog.org
#' - Check whether it exists in glottolog
#'
#' @param glottocode A single glottocode
#'
#'
#' @export
#'
#' @examples
#' glottocode("yucu1253")
glottocode <- function(glottocode){
  if(length(glottocode) > 1){stop("Please provide a single glottocode")}
  if(glottocode_exists(glottocode)){
    glottocode_location(glottocode)
    glottocode_online(glottocode)
  } else{
    message("glottocode not found. You may use glottosearch() to search for it. ")
  }
}


#' Show location of glottocode on globe
#'
#' @param glottocode
#'
#'
#' @export
#' @keywords internal
#' @examples
#' glottocode_location("yucu1253")
glottocode_location <- function(glottocode){

  language <- glottofilter(glottocode = glottocode)
  lon0 = sf::st_coordinates(language)[1]
  lat0 = sf::st_coordinates(language)[2]
  language <- s2::as_s2_geography(paste0("POINT(", lon0, " ", lat0, ")") )

  earth = s2::as_s2_geography(TRUE)
  continents = s2::s2_data_countries()
  oceans = s2::s2_difference(earth, s2::s2_union_agg(continents))
  b = s2::s2_buffer_cells(language, 9800000) # visible half
  i = s2::s2_intersection(b, oceans) # visible ocean
  continents = s2::s2_intersection(b, continents)
  plot(sf::st_transform(sf::st_as_sfc(i), paste0("+proj=ortho +lat_0=",lat0, " +lon_0=",lon0) ), col = 'lightblue')
  plot(sf::st_transform(sf::st_as_sfc(continents), paste0("+proj=ortho +lat_0=",lat0, " +lon_0=",lon0) ), col = "lightgrey", add = TRUE)
  plot(sf::st_transform(sf::st_as_sfc(language), paste0("+proj=ortho +lat_0=",lat0, " +lon_0=",lon0) ), col = "darkred", pch = 1, cex = 3, lwd = 2, add = TRUE)
}


#' Check whether a set of glottocodes exist in glottolog
#'
#' @param glottocode A glottocode or character vector of glottocodes
#'
#' @return A logical vector
#' @export
#' @keywords internal
#' @family <glottocheck><glottosearch>
#' @examples
#' glottocode_exists(c("yucu1253"))
#' glottocode_exists(c("yucu1253", "abcd1234"))
glottocode_exists <- function(glottocode){
  glottosearch_exist(search = glottocode, columns = "glottocode")
}

#' Open url in web browser for glottocode
#'
#' @param glottocode
#'
#'
#' @export
#' @family <glottocheck><glottosearch>
#' @keywords internal
#' @examples
#' glottocode_online("yucu1253")
glottocode_online <- function(glottocode){
  url <- paste0("https://glottolog.org/resource/languoid/id/", glottocode)
  utils::browseURL(url)
}

#' Check whether glottosubcodes are valid.
#'
#' This function checks whether a vector of glottosubcodes adheres to the
#' following form: glottocode_group_record. For example: abcd1234_aaa_0001,
#' abcd1234_aaa_0002, abcd1234_bbb_0001, abcd1234_bbb_0002.
#'
#' Specifically, the function checks whether all glottocodes (which are part of the glotosubcodes) are valid, whether 'group' is a character, and whether 'record' is a number.
#'
#'
#' @param glottosubcodes Character vector of glottosubcodes
#' @family <glottocheck><glottosearch>
#' @return Gives warning in case there are issues, and invisibly returns TRUE otherwise.
#' @export
#'
#' @examples
#' glottosubcode_valid(c("yucu1253_aaa_0002", "abcd1234_aaa_0001"))
glottosubcode_valid <- function(glottosubcodes){
  gsc_df <- data.frame(matrix(nrow = length(glottosubcodes), ncol = 4) )
  colnames(gsc_df) <- c("glottosubcode", "glottocode", "group", "n")

  gsc_df[, 1] <- glottosubcodes

  for(i in seq(gsc_df[, 1])){
    gsc_df[i, "glottocode"] <- strsplit(glottosubcodes, split = "_")[[i]][1]
    gsc_df[i, "group"] <- strsplit(glottosubcodes, split = "_")[[i]][2]
    gsc_df[i, "n"] <- strsplit(glottosubcodes, split = "_")[[i]][3]
  }

  glottocodes <- unique(gsc_df[,"glottocode"]) # I use unique here because glottocode_exists is slow, I match values later with %in%
  gc_exists <- glottocode_exists(glottocodes)
  gsc_df[,"gc_exists"] <- gsc_df[,"glottocode"] %in% glottocodes[gc_exists]

  gsc_df[,"group_chr"] <- suppressWarnings(is.na(as.numeric(gsc_df[,"group"])))
  gsc_df[,"n_num"] <- suppressWarnings(!is.na(as.numeric(gsc_df[,"n"])))

  invalid <- gsc_df %>% dplyr::filter(dplyr::if_any( c(.data$gc_exists, .data$group_chr, .data$n_num), is_false))
  invalidgcs <- paste(invalid[,"glottosubcode"], collapse = ", ")

  if(nrow(invalid) != 0){
    message(paste("There are issues with the following glottosubcodes:", invalidgcs))
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }


}



