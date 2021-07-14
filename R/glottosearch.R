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
glottosearch <- function(glottodata = NULL, find, partialmatch = TRUE, columns = NULL){

 if(is.null(glottodata) ){glottodata <- glottobase}
  if(missing(find)){stop("No search term provided, please indicate what you want to search for.")}
  if(length(find) > 1){stop("More than one search term provided, please provide a single term.",
                            call. = FALSE)}
  ifelse(partialmatch == TRUE, ldist <- 0.1, ldist <- 0)

  if(is.null(columns)){
    glottodata_sel <- glottodata
  } else{ glottodata_sel <- dplyr::select(glottodata, all_of(columns))}

    found <- apply(glottodata_sel, MARGIN = 2, simplify = FALSE,
                   FUN = base::agrep, pattern = find, ignore.case = T, value = FALSE, max.distance = ldist)
    rowid <- unique(unlist(found))
    glottodata[rowid, ]

}

#' Check whether one glot exists in glottolog.
#'
#' @return Logical: TRUE/FALSE
glot_exists_one <- function(find, columns){
  # TODO: Do not load glottobase each time. This will be solved if glottodata is added to package.
  existsdf <- glottosearch(glottodata = glottobase, find = find, partialmatch = FALSE, columns = columns)
  ifelse(nrow(existsdf == 1), existing <- TRUE, existing <- FALSE)
}

#' Check whether a glot or vector of glots exist in glottolog
#'
#' @param find Glottocode, name of language, family, etc.
#' @param columns In which column should be searched
#'
#' @return Logical: TRUE/FALSE
#' @export
#'
#' @examples
#' #' glot_exists(find = c("yucu1253", "abcd1234"), columns = "glottocode")
glot_exists <- function(find, columns){
purrr::map2_lgl(.x = find, .y = columns, .f = glot_exists_one)
}

#' Check whether glottocodes exist in glottolog
#'
#' @param glottocode A glottocode or character vector of glottocodes
#'
#' @return A logical vector
#' @export
#'
#' @examples
#' glottocode_exists(c("yucu1253"))
#' glottocode_exists(c("yucu1253", "abcd1234"))
glottocode_exists <- function(glottocode){
  glot_exists(find = glottocode, columns = "id")
}

glot_online <- function(language = NULL, family = NULL, dialect = NULL){

}
