#' Search within glottodata for languages, glottocodes, etc.
#'
#' @param glottodata Any linguistic or cultural dataset. Default is to search within glottolog.
#' @param find Character string to search for, this can be the name of a language, a family, a dialect, a glottocode, isocode.
#' @param partialmatch By default, partial matches will be returned as well. In case you only want exact matches, this argument should be set to FALSE.
#' @param columns By default, the entire dataset is searched, but optionally the search can be limited to specific columns.
#'
#' @return
#' @export
#'
#' @examples
#' glottosearch(find = "Yucuni")
#' glottosearch(find = "Yucuni", columns = "name")
glottosearch <- function(glottodata = "glottolog", find, partialmatch = TRUE, columns = NULL){

 if(glottodata == "glottolog"){glottodata <- get_glottolog("glottolog")}
  if(missing(find)){stop("No search term provided, please indicate what you want to search for.")}
  if(length(find) > 1){stop("More than one search term provided, please provide a single term.",
                            call. = FALSE)}
  ifelse(partialmatch == TRUE, ldist <- 0.1, ldist <- 0)

  if(is.null(columns)){
    glottodata_sel <- glottodata
  } else{ glottodata_sel <- dplyr::select(glottodata, all_of(columns))}

    found <- apply(glottodata_sel, MARGIN = 2, simplify = FALSE,
                   FUN = base::agrep, pattern = find, ignore.case = T, value = FALSE, max.distance = 0.1)
    rowid <- unique(unlist(found))
    glottodata[rowid, ]

}

#' Check whether a glottocode, language, family, or dialect exists in glottolog. This is a wrapper around glotto_search (only exact matches are returned)
#'
#' @return Logical: TRUE/FALSE
#' @export
#'
#' @examples
glot_exists <- function(find, columns = NULL){
  # TOFIX: If partial match = FALSE, only one result should be returned.
  #
  glottosearch(glottodata = "glottolog", find = "yucu1253", partialmatch = FALSE, columns = "id")
}

glot_online <- function(language = NULL, family = NULL, dialect = NULL){

}
