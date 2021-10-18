#' Search within glottodata for languages, glottocodes, etc.
#'
#' @param glottodata Any linguistic or cultural dataset. Default is to search within glottolog.
#' @param find Character string to search for, this can be the name of a language, a family, a dialect, a glottocode, isocode.
#' @param partialmatch By default, partial matches will be returned as well. In case you only want exact matches, this argument should be set to FALSE.
#' @param columns By default, the entire dataset is searched, but optionally the search can be limited to specific columns.
#' @param tolerance In case partialmatch is TRUE: what is the maximum difference between search term and match? Default is 0.1
#'
#' @return
#' @export
#' @family <glottosearch><glottofilter>
#' @examples
#' glottosearch(find = "Yucuni")
#' glottosearch(find = "Yucuni", columns = "name")
#' glottosearch(find = "Yucuni", columns = c("name", "family"))
glottosearch <- function(find, glottodata = NULL, partialmatch = TRUE, columns = NULL, tolerance = NULL){
  if(is.null(tolerance)){tolerance <- 0.1}

 if(is.null(glottodata) ){glottodata <- glottoget_glottobase()}
  if(missing(find)){stop("No search term provided, please indicate what you want to search for.")}
  if(length(find) > 1){stop("More than one search term provided, please provide a single term.",
                            call. = FALSE)}
  ifelse(partialmatch == TRUE, ldist <- tolerance, ldist <- 0)

  if(is.null(columns)){
    glottodata_sel <- glottodata
  } else{ glottodata_sel <- dplyr::select(glottodata, all_of(columns))}

    found <- base::apply(glottodata_sel, MARGIN = 2,
                   FUN = base::agrep, pattern = find, ignore.case = T, value = FALSE, max.distance = ldist)
    rowid <- base::unique(unlist(found))
    glottodata[rowid, ]

}

#' Check whether one glot exists in glottolog.
#'
#' @param find Glottocode, name of language, family, etc.
#' @param columns In which column should be searched
#' @family <glottocheck><glottosearch>
#' @return Logical: TRUE/FALSE
#' @noRd
glot_exists_one <- function(find, columns){
  existsdf <- glottosearch(glottodata = glottoget_glottobase(), find = find, partialmatch = FALSE, columns = columns)
  nrow(existsdf) == 1
}

#' Check whether a glot or vector of glots exist in glottolog
#'
#' This function checks for exact matches, and returns TRUE/FALSE. In case you
#' want a more flexible search, you can use
#' \code{\link[=glottosearch]{glottosearch()}}, in case you want to check
#' whether glottocodes exist/are valid, use
#' \code{\link[=glottocodes_exist]{glottocodes_exist()}}
#'
#' @param find Glottocode, name of language, family, etc.
#' @param columns In which column should be searched
#' @family <glottocheck><glottosearch>
#' @return Logical: TRUE/FALSE
#' @export
#'
#' @examples
#' glot_exists(find = c("yucu1253", "abcd1234"), columns = "glottocode")
glot_exists <- function(find, columns){
purrr::map2_lgl(.x = find, .y = columns, .f = glot_exists_one)
}

#' Check whether glottocodes exist in glottolog
#'
#' @param glottocode A glottocode or character vector of glottocodes
#'
#' @return A logical vector
#' @export
#' @aliases glottocodes_exist
#' @family <glottocheck><glottosearch>
#' @examples
#' glottocode_exists(c("yucu1253"))
#' glottocode_exists(c("yucu1253", "abcd1234"))
glottocode_exists <- glottocodes_exist <- function(glottocode){
  glot_exists(find = glottocode, columns = "glottocode")
}

#' Open url in web browser for glottocode
#'
#' @param glottocode
#'
#' @return
#' @export
#' @family <glottocheck><glottosearch>
#' @examples
#' glottocode_online("yucu1253")
glottocode_online <- function(glottocode){
  url <- paste0("https://glottolog.org/resource/languoid/id/", glottocode)
  utils::browseURL(url)
}

#' Check whether glottosubcodes are valid.
#'
#' This function checks whether a vector of glottosubcodes adheres to the
#' following form: glottocode_group_record. For example: abcd1234_aaa_0001,
#' abcd1234_aaa_0002, abcd1234_bbb_0001, abcd1234_bbb_0002.
#'
#' Specifically, the function checks whether all glottocodes (which are part of the glotosubcodes) are valid, whether 'group' is a character, and whether 'record' is a number.
#'
#'
#' @param glottosubcodes Character vector of glottosubcodes
#' @family <glottocheck><glottosearch>
#' @return
#' @export
#'
#' @examples
#' glottosubcode_valid(c("yucu1253_aaa_0002", "abcd1234_aaa_0001"))
glottosubcode_valid <- function(glottosubcodes){
  gsc_df <- data.frame(matrix(nrow = length(glottosubcodes), ncol = 4) )
  colnames(gsc_df) <- c("glottosubcode", "glottocode", "group", "n")

  gsc_df[, 1] <- glottosubcodes

  for(i in seq(gsc_df[, 1])){
  gsc_df[i, "glottocode"] <- strsplit(glottosubcodes, split = "_")[[i]][1]
  gsc_df[i, "group"] <- strsplit(glottosubcodes, split = "_")[[i]][2]
  gsc_df[i, "n"] <- strsplit(glottosubcodes, split = "_")[[i]][3]
  }

  glottocodes <- unique(gsc_df[,"glottocode"]) # I use unique here because glottocode_exists is slow, I match values later with %in%
  gc_exists <- glottocode_exists(glottocodes)
  gsc_df[,"gc_exists"] <- gsc_df[,"glottocode"] %in% glottocodes[gc_exists]

  gsc_df[,"group_chr"] <- suppressWarnings(is.na(as.numeric(gsc_df[,"group"])))
  gsc_df[,"n_num"] <- suppressWarnings(!is.na(as.numeric(gsc_df[,"n"])))

  invalid <- gsc_df %>% dplyr::filter(dplyr::if_any( c(gc_exists, group_chr, n_num), is_false))
  invalidgcs <- paste(invalid[,"glottosubcode"], collapse = ", ")

  if(nrow(invalid) != 0){message(paste("There are issues with the following glottosubcodes:", invalidgcs))}


}


