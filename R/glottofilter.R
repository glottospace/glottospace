# TODO: glottofilter: Make more flexible country or continent, if user specifies country = "South America" this should also work.

#' Filter glottodata by language, glottocode, etc.
#'
#' By default, the glottolog data will be used to filter from. But in case the user provides glottodata, this will be used.
#'
#' @param glottodata glot or geoglot object obtained with
#'   \code{\link{get_glottolog}} or \code{\link{as_glot}}  or
#'   \code{\link{as_geoglot}}.
#' @param isocodes A character vector of iso639P3codes
#' @param glottocode A character vector of glottocodes
#' @param family_name A character vector of language families
#' @param family_id A character vector of language family IDs
#' @param continent A character vector of continents
#' @param country A character vector of countries
#' @param expression A regular expression
#' @aliases glotto_select glotto_subset
#'
#' @return A smaller glot object or geoglot object containing only the selected features.
#' @export
#'
#' @examples
#' # Get data
#' glottoraw <- gs_data(name = "glottolog")
#' glottodata <- gs_upgrade(data = glottoraw)
#'
#' # Filter glottodata
#' points <- glottofilter(glottodata = glottodata, isocodes = colnames(distmat))
#' points <- glottofilter(glottodata = glottodata, glottocode = "wari1268")
#' points <- glottofilter(glottodata = glottodata, family_name = 'Indo-European')
#' points <- glottofilter(glottodata = glottodata, continent = "South America")
#' points <- glottofilter(glottodata = glottodata, country = c("Colombia", "Venezuela"))
#' points <- glottofilter(glottodata = glottodata, expression = family_name %in% c("Arawakan", "Tucanoan"))

glottofilter <- function(glottodata = NULL, isocodes = NULL,
                      glottocode = NULL, family_name = NULL, family_id = NULL,
                      continent = NULL, country = NULL, expression = NULL){

  # filter glottolog data
  # isocodes: a character vector of isocodes
  # macroarea: a character vector of macroarea(s)
  # ifelse(!is_empty(continent) | !is_empty(country), spatial <- TRUE, spatial <- FALSE)
  # if(spatial == TRUE){
  #   if (!require(rnaturalearth)) {install.packages('rnaturalearth')}
  #   library(rnaturalearth)
  #   # TO ADD: check CRS identical
  #   }

  if(purrr::is_empty(glottodata)){
    glottodata <- get_glottobase()
  }

  # Filter by expression:
  expression <- base::substitute(expression)
  if(!purrr::is_empty(as.character(expression))){
    glottodata <- dplyr::filter(glottodata, eval(expression))
  }

  if(!purrr::is_empty(isocodes)){
    selection <- isocodes
    glottodata <- glottodata %>%
      dplyr::filter(isocode %in% selection)
  }
  if(!purrr::is_empty(glottocode)){
    selection <- glottocode
    glottodata <- glottodata %>%
      dplyr::filter(glottocode %in% selection)
  }
  if(!purrr::is_empty(family_name)){
    selection <- family_name
    glottodata <- glottodata %>%
      dplyr::filter(family_name %in% selection)
  }
  if(!purrr::is_empty(family_id)){
    selection <- family_id
    glottodata <- glottodata %>%
      dplyr::filter(family_id %in% selection)
  }
  # if (sum( (!is.null(country)) + (!is.null(continent)) ) > 1) {
  #   stop("Please supply either country or continent, not both")
  # }
  if(!purrr::is_empty(continent)){
    selection <- continent
    glottodata <- glottodata %>%
      dplyr::filter(continent %in% selection)
  }
  if(!purrr::is_empty(country)){
    selection <- country
    glottodata <- glottodata %>%
      dplyr::filter(country %in% selection)
  }
  return(glottodata)
}
