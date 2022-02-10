#' Search within glottodata for languages, glottocodes, etc.
#'
#' @param glottodata Any linguistic or cultural dataset. Default is to search within glottobase.
#' @param search Character string to search for, this can be the name of a language, a family, a glottocode, isocode.
#' @param partialmatch By default, partial matches will be returned as well. In case you only want exact matches, this argument should be set to FALSE.
#' @param columns By default, the entire dataset is searched, but optionally the search can be limited to specific columns.
#' @param tolerance In case partialmatch is TRUE: what is the maximum difference between search term and match? Default is 0.1
#' @return A subset of glottodata that matches search conditions (object returned as a data.frame/tibble)
#' @export
#' @family <glottosearch><glottofilter>
#' @examples
#' \donttest{
#' glottosearch(search = "Yucuni")
#' glottosearch(search = "Yucuni", columns = "name")
#' glottosearch(search = "Yucuni", columns = c("name", "family"))
#' }
glottosearch <- function(search, glottodata = NULL, partialmatch = TRUE, columns = NULL, tolerance = NULL){
  if(is.null(tolerance)){tolerance <- 0.1}

 if(is.null(glottodata) ){glottodata <- glottoget_glottobase()}
  if(missing(search)){stop("No search term provided, please indicate what you want to search for.")}
  if(length(search) > 1){stop("More than one search term provided, please provide a single term.",
                            call. = FALSE)}
  ifelse(partialmatch == TRUE, ldist <- tolerance, ldist <- 0)

  if(is.null(columns)){
    glottodata_sel <- glottodata
  } else{ glottodata_sel <- dplyr::select(glottodata, dplyr::all_of(columns))}

    found <- base::apply(glottodata_sel, MARGIN = 2,
                   FUN = base::agrep, pattern = search, ignore.case = T, value = FALSE, max.distance = ldist)
    rowid <- base::unique(unlist(found))
    glottodata[rowid, ]

}

#' Check whether a single keyword exists in glottolog.
#'
#' @param search Glottocode, name of language, family, etc.
#' @param columns In which column should be searched
#' @family <glottocheck><glottosearch>
#' @return Logical: TRUE/FALSE
#' @noRd
glottosearch_1valid <- function(search, columns){
  existsdf <- glottosearch(glottodata = glottoget_glottobase(), search = search, partialmatch = FALSE, columns = columns)
  nrow(existsdf) == 1
}

#' Check whether a keyword or vector of keywords exist in glottolog
#'
#' This function checks for exact matches, and returns TRUE/FALSE. In case you
#' want a more flexible search, you can use
#' \code{\link[=glottosearch]{glottosearch()}}, in case you want to check
#' whether glottocodes exist/are valid, use
#' \code{\link[=glottocode_exists]{glottocode_exists()}}
#'
#' @param search glottocode, name of language, family, etc.
#' @param columns In which column should be searched
#' @family <glottocheck><glottosearch>
#' @return Logical: TRUE/FALSE
#' @noRd
#' @examples
#' glottosearch_exist(search = c("yucu1253", "abcd1234"), columns = "glottocode")
glottosearch_exist <- function(search, columns){
purrr::map2_lgl(.x = search, .y = columns, .f = glottosearch_1valid)
}



