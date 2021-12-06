#' Filter glottodata by language, glottocode, etc.
#'
#' By default, the glottolog data will be used to filter from. But in case the user provides glottodata, this will be used.
#'
#' @param glottodata A glottodata table
#' @param isocodes A character vector of iso639p3codes
#' @param glottocode A character vector of glottocodes
#' @param family A character vector of language families
#' @param family_id A character vector of language family IDs
#' @param continent A character vector of continents
#' @param country A character vector of countries
#' @param name A character vector of language names
#' @param region A character vector of regions
#' @param colname A column name
#' @param keep Character vector of things to keep (only if colname is provided)
#' @param drop Character vector of things to keep (only if colname is provided)
#' @param expression A regular expression
#'
#' @family <glottofilter><glottosearch>
#' @return A subset of the original glottodata table  containing only filtered languages.
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
#' points <- glottofilter(glottodata = glottodata, family = "Indo-European")
#' points <- glottofilter(glottodata = glottodata, continent = "South America")
#' points <- glottofilter(glottodata = glottodata, family = "Indo-European", continent = "South America")
#' points <- glottofilter(glottodata = glottodata, country = c("Colombia", "Venezuela"))
#' points <- glottofilter(glottodata = glottodata, expression = family %in% c("Arawakan", "Tucanoan"))
#' points <- glottofilter(glottodata = glottodata, colname = "family", drop = "Indo-European")
glottofilter <- function(glottodata = NULL, isocodes = NULL,
                      glottocode = NULL, name = NULL, family = NULL, family_id = NULL,
                      continent = NULL, country = NULL, region = NULL, expression = NULL, colname = NULL, keep = NULL, drop = NULL){

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
    glottodata <- glottoget_glottobase()
  }

  # Filter by expression:
  expression <- base::substitute(expression)
  if(!purrr::is_empty(expression)){
    glottodata <- dplyr::filter(glottodata, eval(expression))
  }

  if(!purrr::is_empty(isocodes )){
    selection <- isocodes
    glottodata <- glottodata %>%
      dplyr::filter(isocode %in% selection)
  }
  if(!purrr::is_empty(glottocode )){
    selection <- glottocode
    glottodata <- glottodata %>%
      dplyr::filter(glottocode %in% selection)
  }
  if(!purrr::is_empty(family )){
    selection <- family
    glottodata <- glottodata %>%
      dplyr::filter(family %in% selection)
  }
  if(!purrr::is_empty(name )){
    selection <- name
    glottodata <- glottodata %>%
      dplyr::filter(name %in% selection)
  }
  if(!purrr::is_empty(family_id )){
    selection <- family_id
    glottodata <- glottodata %>%
      dplyr::filter(family_id %in% selection)
  }
  # if (sum( (!is.null(country)) + (!is.null(continent)) ) > 1) {
  #   stop("Please supply either country or continent, not both")
  # }
  if(!purrr::is_empty(continent )){
    selection <- continent
    glottodata <- glottodata %>%
      dplyr::filter(continent %in% selection)
  }
  if(!purrr::is_empty(country )){
    selection <- country
    glottodata <- glottodata %>%
      dplyr::filter(country %in% selection)
  }
  if(!purrr::is_empty(region )){
    selection <- region
    glottodata <- glottodata %>%
      dplyr::filter(region %in% selection)
  }

  if(!purrr::is_empty(colname) & !purrr::is_empty(keep) ){
    # https://stackoverflow.com/questions/27197617/filter-data-frame-by-character-column-name-in-dplyr
    selection <- keep
    glottodata <- glottodata %>%
      dplyr::filter(.[[colname]] %in% selection)
  }

  if(!purrr::is_empty(colname) & !purrr::is_empty(drop) ){
    # https://stackoverflow.com/questions/27197617/filter-data-frame-by-character-column-name-in-dplyr
    selection <- drop
    glottodata <- glottodata %>%
      dplyr::filter(.[[colname]] %nin% selection)
  }

  if(nrow(glottodata) == 0){
    message("No search results. Use glottosearch() first to find what you're looking for")
  } else{
  return(glottodata)
  }
}



# arg <- base::quote("Indo-European") # works
# arg <- base::quote(-"Indo-European") # works
# arg <- base::quote(c("Germany", "Netherlands") ) # works
# arg <- base::quote(-c("Germany", "Netherlands") ) # works

#' glottofilter by column name
#'
#' This is a low-level function of glottofilter, allowing for negative selections
#'
#' @param glottodata glottodata table
#' @param colname Character with a single column name
#' @param select Things to be selected in that column
#'
#' @return
#' @export
#'
#' @examples
#' glottodata <- glottoget("glottobase")
#' glottofilter_bycol(glottodata = glottodata, colname = "family", select = "Indo-European")
#' glottofilter_bycol(glottodata = glottodata, colname = "family", select = -"Indo-European")
#' glottofilter_bycol(glottodata = glottodata, colname = "country", select = c("Germany", "Netherlands") )
#' glottofilter_bycol(glottodata = glottodata, colname = "country", select = -c("Germany", "Netherlands") )
glottofilter_bycol <- function(glottodata, colname, select){

arg <- base::substitute(select)
char <- as.character(arg)

if(char[1] == "-"){ # inverse
  if(substring(char[2], 1, 2) == "c("){ # inverse multiple
    selection <- gsub("[[:punct:]]", " ", char[2])
    selection <- stringr::str_split(selection, " ")
    selection <- selection[[1]]
    selection <- selection[selection %nin% c("c", "")]
  } else {
    selection <- char[2]
  }
  glottodata <- glottodata %>%
    dplyr::filter(.[[colname]] %nin% selection)
} else { # no inverse
  selection <- select
  glottodata <- glottodata %>%
    dplyr::filter(.[[colname]] %in% selection)
}

return(glottodata)
}


