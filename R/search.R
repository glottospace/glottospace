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
glotto_search <- function(glottodata = "glottolog", find = NULL, partialmatch = TRUE){

 if(glottodata == "glottolog"){glottodata <- get_glottolog("glottolog")}

  if(partial == TRUE){
    rowid <- agrep("Yucuna",glottodata$name,ignore.case=T,value=FALSE,max.distance = 0.1)
    glottodata[rowid, ]

    found <- apply(glottodata, MARGIN = 2, simplify = FALSE, FUN = base::agrep, pattern = find, ignore.case = T, value = FALSE, max.distance = 0.1)
    lapply(found, sjmisc::is_empty)
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
