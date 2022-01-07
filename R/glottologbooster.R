#' Glottologbooster: enhance glottolog data
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with \code{\link{glottoget_glottolog()}}
#' @param space Return spatial object?
#' @param addfamname Add column with familiy names?
#' @param addisolates Add column to identify isolates?
#' @param L1only Keep only L1 languages (remove bookkeeping, unclassifiable, sign languges, etc.). See glottologbooster_rm for more flexibility.
#' @param addfamsize Add column with family size?
#' @param addfamsizerank Add column with family size rank?
#'
#' @return
#' @export
#' @keywords internal
#' @examples
#' glottologdata <- glottoget("glottolog")
#' glottologdata <- glottologbooster(glottologdata)
glottologbooster <- function(glottologdata = NULL, space = TRUE,
                                addfamname = TRUE, addisolates = TRUE,
                                L1only = TRUE,
                                addfamsize = TRUE, addfamsizerank = TRUE){
  if(is.null(glottologdata)){
    glottologdata <- glottoget_glottolog()
  }
  if(addfamname == TRUE){glottologdata <- glottolog_addfamilyname(glottologdata) }
  if(addisolates == TRUE){glottologdata <- glottolog_addisolates(glottologdata) }

  if(L1only == TRUE){glottologdata <- glottolog_L1only(glottologdata) }

  if(addfamsize == TRUE){glottologdata <- glottolog_addfamilysize(glottologdata) }
  if(addfamsizerank == TRUE){glottologdata <- glottolog_addfamilysizerank(glottologdata) }

  glottologdata <- glottologdata %>% dplyr::rename("glottocode" = "id", "isocode" = "iso639p3code")

  if(space == TRUE){
    glottologdata <- glot2geoglot(glottologdata)
    glottologdata <- glottodata_addcountries(glottologdata)
  }
  return(glottologdata)
}

#' Glottolog: add family name
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with \code{\link{glottoget_glottolog()}}
#' @keywords internal
#' @family <glottolog>
#' @return
#' @export
glottolog_addfamilyname <- function(glottologdata){
    families <- glottologdata %>%
      dplyr::filter(level == "family") %>%
      dplyr::transmute(family_id = id, family = name)

    dplyr::left_join(x = glottologdata, y = families, by = "family_id")
}

#' Glottolog: add isolates
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with \code{\link{glottoget_glottolog()}}
#' @keywords internal
#' @family <glottolog>
#' @return
#' @export
glottolog_addisolates <- function(glottologdata){

    glottologdata$isolate <-   ifelse(
      ( (glottologdata$family_id == "") & (glottologdata$level != "family")  ),
      TRUE, FALSE)
    # # assign language name to family
    glottologdata[glottologdata$isolate == TRUE, "family"] <- glottologdata[glottologdata$isolate == TRUE, "name"]
    # assign glottocode to family_id
    glottologdata[glottologdata$isolate == TRUE, "family_id"] <- glottologdata[glottologdata$isolate == TRUE, "id"]

glottologdata
}

#' Glottolog: remove dialects
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with \code{\link{glottoget_glottolog()}}
#' @keywords internal
#' @family <glottolog>
#' @return
#' @export
glottolog_rmdialects <- function(glottologdata){
  glottologdata %>% dplyr::filter(level != "dialect")
}

#' Glottolog: remove families
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with \code{\link{glottoget_glottolog()}}
#' @keywords internal
#' @family <glottolog>
#' @return
#' @export
glottolog_rmfamilies <- function(glottologdata){
  glottologdata %>% dplyr::filter(level != "family")
}

#' Glottolog: remove bookkeeping
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with \code{\link{glottoget_glottolog()}}
#' @keywords internal
#' @family <glottolog>
#' @return
#' @export
glottolog_rmbookkeeping <- function(glottologdata){
  # unique(glottologdata$bookkeeping)
  glottologdata %>%
    dplyr::filter(bookkeeping == FALSE) %>%
    dplyr::select(-bookkeeping)
}

#' Glottolog: remove artificial families
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with \code{\link{glottoget_glottolog()}}
#' @keywords internal
#' @family <glottolog>
#' @return
#' @export
glottolog_rmartifam <- function(glottologdata){
  glottologdata %>%
    dplyr::filter(family_id != "arti1236")
}

#' Glottolog: remove sign languages
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with \code{\link{glottoget_glottolog()}}
#' @keywords internal
#' @family <glottolog>
#' @return
#' @export
glottolog_rmsignlangs <- function(glottologdata){
  glottologdata %>%
    dplyr::filter(family_id != "sign1238")
}

#' Glottolog: add family size
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with \code{\link{glottoget_glottolog()}}
#' @keywords internal
#' @family <glottolog>
#' @return
#' @export
glottolog_addfamilysize <- function(glottologdata){
glottologdata %>%
    dplyr::group_by(family_id) %>%
    dplyr::mutate(family_size = dplyr::n())
}

#' Glottolog: add rank order of family size
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with \code{\link{glottoget_glottolog()}}
#' @keywords internal
#' @family <glottolog>
#' @return
#' @export
glottolog_addfamilysizerank <- function(glottologdata){
  glottologdata$family_size_rank <- as.factor(glottologdata$family_size)
  levels(glottologdata$family_size_rank) <- seq(1:length(levels(glottologdata$family_size_rank)))
  glottologdata$family_size_rank  <- as.numeric(glottologdata$family_size_rank) # easier plotting than ordered levels

  glottologdata
}

#' Glottolog: keep only L1 languages
#'
#' Filter out only L1 languages and drop 'category' column.
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with \code{\link{glottoget_glottolog()}}
#' @keywords internal
#' @family <glottolog>
#' @return
#' @export
glottolog_L1only <- function(glottologdata){
  glottologdata %>%
    dplyr::filter(category == "Spoken_L1_Language") %>%
    dplyr::select(-category)
}


