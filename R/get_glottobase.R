#' Get glottobase reference data
#'
#' Downloads most recent glottolog data and transforms it. This 'glottobase' is used as reference dataset in several functions.
#'
#' @return
#' @export
#'
#' @examples
#' glottobase <- get_glottobase()
get_glottobase <- function(){
  glottolog <- get_glottolog(data = "glottolog")
  glottobase <- glottologbooster(glottologdata = glottolog)
  glottobase
}

#' Get glottospace reference data
#'
#' Get most recent glottolog data and turn it into the most elemental geoglot object (i.e. glottocodes + geometry column). This 'glottospace' is used as reference dataset in several functions.
#'
#' @return
#' @export
#' @seealso glottodata_addcoords
#'
#' @examples
#' glottospace <- get_glottospace()
get_glottospace <- function(){
  glottologdata <- get_glottolog(data = "glottolog")
  glottologdata <- glottologdata %>% dplyr::rename("glottocode" = "id")
  glottospace <- glot2geoglot(glottologdata)
  glottospace <- glottospace[,c("glottocode")]
}
