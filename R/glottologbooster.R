#' Glottologbooster: enhance glottolog data
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with \code{\link{glottoget_path("glottolog")}}
#'
#' @return
#' @export
#'
#' @examples
#' glottologdata <- glottoget_path("glottolog")
#' glottologdata <- glottologbooster(glottologdata)
glottologbooster <- function(glottologdata, geoglot = TRUE){
  glottologdata <- glottolog_addfamilyname(glottologdata)
  glottologdata <- glottolog_addisolates(glottologdata)
  glottologdata <- glottolog_rmdialects(glottologdata)
  glottologdata <- glottolog_rmfamilies(glottologdata)
  glottologdata <- glottolog_addfamilysize(glottologdata)
  glottologdata <- glottolog_addfamilysizerank(glottologdata)
  glottologdata <- glottolog_rmbookkeeping(glottologdata)
  glottologdata <- glottolog_rmartifam(glottologdata)
  glottologdata <- glottolog_rmsignlangs(glottologdata)

  glottologdata <- glottologdata %>% dplyr::rename("glottocode" = "id", "isocode" = "iso639P3code")

  glottologdata <- glottologdata %>% dplyr::select(-c(level, description,
                                                      markup_description,
                                                      child_family_count,
                                                      child_language_count))
  if(geoglot == TRUE){
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
      dplyr::transmute(family_id = id, family_name = name)

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
      ( (glottologdata$family_id == "") & (glottologdata$level != "family") ),
      TRUE, FALSE)
    # # assign language name to family_name
    glottologdata[glottologdata$isolate == TRUE, "family_name"] <- glottologdata[glottologdata$isolate == TRUE, "name"]
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
    dplyr::filter(bookkeeping == "False") %>%
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


