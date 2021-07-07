#' Filter glottolog data
#'
#' @param glottodata glot or geoglot object obtained with
#'   \code{\link{glotto_get}} or \code{\link{as_glot}}  or
#'   \code{\link{as_geoglot}}.
#' @param isocodes A character vector of isocodes
#' @param glottocode A character vector of glottocodes
#' @param family_name A character vector of language families
#' @param family_id A character vector of language family IDs
#' @param continent A character vector of continents
#' @param country A character vector of countries
#' @param expression A regular expression
#' @aliases glotto_select
#' @aliases glotto_subset
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
#' points <- glotto_filter(data = glottodata, isocodes = colnames(distmat))
#' points <- glotto_filter(data = glottodata, glottocode = "wari1268")
#' points <- glotto_filter(data = glottodata, family_name = 'Indo-European')
#' points <- glotto_filter(data = glottodata, continent = "South America")
#' points <- glotto_filter(data = glottodata, country = c("Colombia", "Venezuela"))
#' points <- glotto_filter(data = glottodata, expression = family_name %in% c("Arawakan", "Tucanoan"))

glotto_filter <- function(glottodata = NULL, isocodes = NULL,
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

  if(is_empty(data)){
    message("No input data provided, glottolog data downloaded and transformed")
    data <- gs_data()
    data <- gs_transform(data)
  }

  # Filter by expression:
  expression <- substitute(expression)
  if(!is_empty(as.character(expression))){
    data <- filter(data, eval(expression))
  }

  if(!is_empty(isocodes)){
    selection <- isocodes
    data <- data %>%
      filter(isocode %in% selection)
  }
  if(!is_empty(glottocode)){
    selection <- glottocode
    data <- data %>%
      filter(glottocode %in% selection)
  }
  if(!is_empty(family_name)){
    selection <- family_name
    data <- data %>%
      filter(family_name %in% selection)
  }
  if(!is_empty(family_id)){
    selection <- family_id
    data <- data %>%
      filter(family_id %in% selection)
  }
  # if (sum( (!is.null(country)) + (!is.null(continent)) ) > 1) {
  #   stop("Please supply either country or continent, not both")
  # }
  if(!is_empty(continent)){
    selection <- continent
    data <- data %>%
      filter(continent %in% selection)
  }
  if(!is_empty(country)){
    selection <- country
    data <- data %>%
      filter(country %in% selection)
  }
  return(data)
}

