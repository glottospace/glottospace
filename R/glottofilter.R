#' Filter glottodata by language, glottocode, etc.
#'
#' By default, the glottolog data will be used to filter from. But in case the user provides glottodata, this will be used.
#'
#' @param glottodata A glottodata table
#' @param isocodes A character vector of iso639p3codes
#' @param glottocode A character vector of glottocodes
#' @param family A character vector of language families
#' @param location A character vector with a location (either a continent, country, macroarea, or sovereignty)
#' @param family_id A character vector of language family IDs
#' @param continent A character vector of continents
#' @param country A character vector of countries
#' @param name A character vector of language names
#' @param colname A column name
#' @param select Character vector of things to select (only if colname is provided)
#' @param drop Character vector of things to drop (only if colname is provided)
#' @param expression A logical expression
#' @param sovereignty Sovereignty
#' @param macroarea Glottolog macroarea
#'
#' @family <glottofilter><glottosearch>
#' @seealso glottofiltermap()
#' @return A subset of the original glottodata table (data.frame or sf) containing only filtered languages.
#' @export
#'
#' @examples
#' \donttest{
#' points <- glottofilter(location = "Australia")
#' points <- glottofilter(glottocode = "wari1268")
#' points <- glottofilter(family = "Indo-European")
#' points <- glottofilter(continent = "South America")
#' points <- glottofilter(family = "Indo-European", continent = "South America")
#' points <- glottofilter(country = c("Colombia", "Venezuela"))
#' points <- glottofilter(expression = family %in% c("Arawakan", "Tucanoan"))
#' points <- glottofilter(expression = family_size > 2)
#' points <- glottofilter(colname = "family", drop = "Indo-European")
#' }
glottofilter <- function(glottodata = NULL,
                      glottocode = NULL, location = NULL, name = NULL, family = NULL, family_id = NULL,
                      continent = NULL, country = NULL, sovereignty = NULL, macroarea = NULL, expression = NULL, isocodes = NULL,colname = NULL, select = NULL, drop = NULL){

  if(purrr::is_empty(glottodata)){
    glottodata <- glottoget_glottobase()
  }

  expression <- base::substitute(expression)
  if(!purrr::is_empty(expression)){
    glottodata <- dplyr::filter(glottodata, eval(expression))
  }

  if(!purrr::is_empty(isocodes )){
    selection <- isocodes
    glottodata <- glottodata %>%
      dplyr::filter(.data$isocode %in% selection)
  }
  if(!purrr::is_empty(glottocode )){
    selection <- glottocode
    glottodata <- glottodata %>%
      dplyr::filter(.data$glottocode %in% selection)
  }
  if(!purrr::is_empty(family )){
    selection <- tolower(family)
    glottodata <- glottodata %>%
      dplyr::filter(tolower(family) %in% selection)
  }
  if(!purrr::is_empty(name )){
    selection <- tolower(name)
    glottodata <- glottodata %>%
      dplyr::filter(tolower(name) %in% selection)
  }
  if(!purrr::is_empty(family_id )){
    selection <- family_id
    glottodata <- glottodata %>%
      dplyr::filter(.data$family_id %in% selection)
  }
  if(!purrr::is_empty(location )){
    selection <- tolower(location)
    # lower case:
    glottodatalower <- glottodata %>% dplyr::select(c(glottocode, continent, macroarea, country, sovereignty)) %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), tolower))

    glottocodesel <- glottodatalower %>%
      dplyr::filter(
        dplyr::if_any(
          dplyr::everything(),
          ~ .x %in% selection)) %>%
      dplyr::select(glottocode)

    glottodata <- glottodata %>%
      dplyr::filter(glottocode %in% glottocodesel[,])
  }
  if(!purrr::is_empty(macroarea )){
    selection <- tolower(macroarea)
    glottodata <- glottodata %>%
      dplyr::filter(tolower(macroarea) %in% selection)
  }
  if(!purrr::is_empty(continent )){
    selection <- tolower(continent)
    glottodata <- glottodata %>%
      dplyr::filter(tolower(continent) %in% selection)
  }
  if(!purrr::is_empty(country )){
    selection <- tolower(country)
    glottodata <- glottodata %>%
      dplyr::filter(tolower(country) %in% selection)
  }
  if(!purrr::is_empty(sovereignty )){
    selection <- tolower(sovereignty)
    glottodata <- glottodata %>%
      dplyr::filter(tolower(sovereignty) %in% selection)
  }

  if(!purrr::is_empty(colname) & !purrr::is_empty(select) ){
    selection <- select
    glottodata <- glottodata %>%
      dplyr::filter(.data[[colname]] %in% selection)
  }

  if(!purrr::is_empty(colname) & !purrr::is_empty(drop) ){
    selection <- drop
    glottodata <- glottodata %>%
      dplyr::filter(.data[[colname]] %nin% selection)
  }

  if(nrow(glottodata) == 0){
    message("No search results. You might consider using glottosearch() first")
    if(purrr::is_empty(location) & !purrr::is_empty(c(country, continent, macroarea, sovereignty) )){message("or use glottofilter(location= )")}
  } else{
  return(glottodata)
  }
}

#' Filter languages interactively from a map
#'
#' Select languages by drawing or clicking on a map. The output should be assigned to a new object. In case you want to select languages based on a (non-spatial) condition, you might want to use glottofilter() instead.
#'
#' @param glottodata Spatial glottodata object
#' @param mode You can choose here whether you want to interactively select languages by clicking on them (mode = 'click', default) or by drawing a shape around them (mode = 'draw').
#' @param ... Additional arguments to pass to glottofilter
#' @export
#' @return A set of languages selected from the original glottodata object
#'
#' @examples
#' \dontrun{
#' # Interactive selection by clicking on languages:
#' selected <- glottofiltermap(continent = "South America")
#' glottomap(selected)
#'
#' # Interactive selection by drawing a shape:
#' selected <- glottofiltermap(continent = "South America", mode = "draw")
#' glottomap(selected)
#' }
glottofiltermap <- function(glottodata = NULL, mode = NULL, ...){
  rlang::check_installed("mapedit", reason = "to use `glottofiltermap()`")

  if(purrr::is_empty(glottodata)){
    glottodata <- glottofilter(...)
  }

  if(is.null(mode)){mode <- "click"}

  suppressMessages(mapedit::selectFeatures(glottodata, mode = mode, title = "Select languages"))
}

#' Flexible version of glottofilter
#'
#' This is a more flexible alternative to glottofilter, allowing for negative selections.
#'
#'
#' @param glottodata glottodata table
#' @param colname Character with a single column name
#' @param select Things to be selected in that column
#' @importFrom rlang .data
#' @export
#' @keywords internal
#' @examples
#' \donttest{
#' glottodata <- glottoget("glottobase")
#' glottofilterflex(glottodata = glottodata, colname = "family",
#' select = "Indo-European")
#' glottofilterflex(glottodata = glottodata, colname = "family",
#' select = -"Indo-European")
#' glottofilterflex(glottodata = glottodata, colname = "country",
#' select = c("Germany", "Netherlands") )
#' glottofilterflex(glottodata = glottodata, colname = "country",
#' select = -c("Germany", "Netherlands") )
#' glottofilterflex(glottodata = glottodata, colname = "continent",
#' select = -c("South America", "Europe", "Asia", "Oceania", "Africa") )
#' }
glottofilterflex <- function(glottodata, colname, select){

  if(purrr::is_empty(glottodata)){
    glottodata <- glottoget_glottobase()
  }

arg <- base::substitute(select)
char <- as.character(arg)

if(char[1] == "-"){ # inverse
  if(substring(char[2], 1, 2) == "c("){ # inverse multiple
    selection <- gsub(pattern = "[[:punct:]]", replacement = ",", x = char[2])
    selection <- strsplit(x = selection, split = ",")
    selection <- selection[[1]]
    selection <- selection[selection %nin% c("c", " ", "")]
  } else {
    selection <- char[2]
  }
  glottodata <- glottodata %>%
    dplyr::filter(.data[[colname]] %nin% selection)
} else { # no inverse
  selection <- select
  glottodata <- glottodata %>%
    dplyr::filter(.data[[colname]] %in% selection)
}

return(glottodata)
}


