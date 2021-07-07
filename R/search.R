#' Title
#'
#' @param name Name of a language, family, or dialect
#' @param partial Whether partial matches should be returned besides exact matches.
#' @param isocode Three letter isocode(s): "abc" or c("abc", "abd")
#' @param glottocode Glottocode(s): "abcd1234" or c("abcd1234", "abcd1235")
#' @param webpage Logical, whether
#' @param logical
#'
#' @return
#' @export
#'
#' @examples
glotto_search <- function(glottodata = "glottolog", name = NULL, partial = TRUE, isocode = NULL, glottocode = NULL){
  #' @importFrom data.table %like%
 if(glottodata == "glottolog"){glottodata <- get_glottolog("glottolog")}

  if(partial == TRUE){
  # glottodata$name %like%
  }
}

#' Check whether a glottocode, language, family, or dialect exists in glottolog. This is a wrapper around glotto_search (only exact matches are returned)
#'
#' @return
#' @export
#'
#' @examples
glot_exists <- function(glottocode = NULL, language = NULL, family = NULL, dialect = NULL){

}

glot_online <- function(language = NULL, family = NULL, dialect = NULL){

}
