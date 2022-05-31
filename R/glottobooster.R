#' Enhance glottolog data
#'
#' This function restructures glottolog data, and optionally adds/removes data. If you want more flexibility in choosing which data to add/remove, you can use glottoboosterflex().
#'
#' This function is used to generate 'glottobase' (the reference dataset used throughout the glottospace R package). The default options generate 'glottobase', which can be loaded directly using glottoget("glottobase").
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with glottoget("glottolog").
#' @param space Return spatial object?
#' @param addfamname Add column with familiy names?
#' @param addisolates Add column to identify isolates?
#' @param L1only Keep only L1 languages (remove bookkeeping, unclassifiable, sign languages, etc.).
#' @param addfamsize Add column with family size?
#' @param addfamsizerank Add column with family size rank?
#' @param rename Rename columns "id" to "glottocode" and "iso639p3code" to "isocode"
#'
#' @family <glottobooster>
#' @return glottologdata object, either a spatial object (class: sf) or a data.frame.
#' @export
#' @examples
#' \donttest{
#' glottologdata <- glottoget("glottolog")
#' glottobase <- glottobooster(glottologdata)
#' }
glottobooster <- function(glottologdata = NULL, space = TRUE,
                                addfamname = TRUE, addisolates = TRUE,
                                L1only = TRUE,
                                addfamsize = TRUE, addfamsizerank = TRUE,
                          rename = TRUE){
  if(is.null(glottologdata)){
    glottologdata <- glottospace::glottolog
  }
  if(addfamname == TRUE){glottologdata <- glottobooster_addfamilyname(glottologdata) }
  if(addisolates == TRUE){glottologdata <- glottobooster_addisolates(glottologdata) }

  if(L1only == TRUE){glottologdata <- glottobooster_L1only(glottologdata) }

  if(addfamsize == TRUE){glottologdata <- glottobooster_addfamilysize(glottologdata) }
  if(addfamsizerank == TRUE){glottologdata <- glottobooster_addfamilysizerank(glottologdata) }

  if(rename == TRUE){
  glottologdata <- glottologdata %>% dplyr::rename("glottocode" = "id", "isocode" = "iso639p3code")
  }

  if(space == TRUE){
    glottologdata <- glottospace_coords2sf(glottologdata)
    glottologdata <- glottospace_addcountries(glottologdata)
  }
  return(glottologdata)
}

#' Enhance glottolog data (flexible options)
#'
#' It is recommended to use glottobooster, but this function is more flexible in removing/adding columns.
#'
#' Note that the different options are additional. For example, if you set rmfamilies to TRUE and space = TRUE, most families will still be removed since they lack spatial coordinates.
#' Another example, depending on whether artificial families are removed, the total number of families also increases/decreases.
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with glottoget("glottolog").
#' @param space Return spatial object?
#' @param addfamname Add column with familiy names?
#' @param addisolates Add column to identify isolates?
#' @param rmdialects Remove rows with dialects?
#' @param rmfamilies Remove rows with families?
#' @param addfamsize Add column with family size?
#' @param addfamsizerank Add column with family size rank?
#' @param rmbookkeeping Remove bookkeeping rows and delete bookkeeping column?
#' @param rmartifam Remove rows that refer to artificial families?
#' @param rename Rename columns "id" to "glottocode" and "iso639p3code" to "isocode"
#' @param rmsignlangs Remove rows with sign languages?
#'
#' @family <glottobooster>
#' @export
#' @keywords internal
#' @return glottologdata object, either a spatial object (class: sf) or a data.frame.
#' @examples
#' \donttest{
#' glottologdata <- glottoget("glottolog")
#' glottologdata <- glottoboosterflex(glottologdata)
#' }
glottoboosterflex <- function(glottologdata = NULL, space = TRUE,
                             addfamname = TRUE, addisolates = TRUE,
                             rmdialects = TRUE, rmfamilies = TRUE,
                             addfamsize = TRUE, addfamsizerank = TRUE,
                             rmbookkeeping = TRUE, rmartifam = TRUE,
                             rmsignlangs = TRUE, rename = TRUE){
  if(is.null(glottologdata)){
    glottologdata <- glottoget_glottolog()
  }
  if(addfamname == TRUE){glottologdata <- glottobooster_addfamilyname(glottologdata) }
  if(addisolates == TRUE){glottologdata <- glottobooster_addisolates(glottologdata) }

  if(rmbookkeeping == TRUE){glottologdata <- glottobooster_rmbookkeeping(glottologdata) }
  if(rmartifam == TRUE){glottologdata <- glottobooster_rmartifam(glottologdata) }
  if(rmsignlangs == TRUE){glottologdata <- glottobooster_rmsignlangs(glottologdata) }
  if(rmdialects == TRUE){glottologdata <- glottobooster_rmdialects(glottologdata) }
  if(rmfamilies == TRUE){glottologdata <- glottobooster_rmfamilies(glottologdata) }

  if(addfamsize == TRUE){glottologdata <- glottobooster_addfamilysize(glottologdata) }
  if(addfamsizerank == TRUE){glottologdata <- glottobooster_addfamilysizerank(glottologdata) }

  if(rename == TRUE){
  glottologdata <- glottologdata %>% dplyr::rename("glottocode" = "id", "isocode" = "iso639p3code")
  }

  if(space == TRUE){
    glottologdata <- glottospace_coords2sf(glottologdata)
    glottologdata <- glottospace_addcountries(glottologdata)
  }
  return(glottologdata)
}

#' Glottolog: add family name
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with glottoget("glottolog").
#' @noRd
#' @family <glottobooster>
#'
glottobooster_addfamilyname <- function(glottologdata){
    families <- glottologdata %>%
      dplyr::filter(.data$level == "family") %>%
      dplyr::transmute(family_id = .data$id, family = .data$name)

    dplyr::left_join(x = glottologdata, y = families, by = "family_id")
}

#' Glottolog: add isolates
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with glottoget("glottolog")
#' @noRd
#' @family <glottobooster>
#'
glottobooster_addisolates <- function(glottologdata){

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
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with glottoget("glottolog")
#' @noRd
#' @family <glottobooster>
#'
glottobooster_rmdialects <- function(glottologdata){
  glottologdata %>% dplyr::filter(.data$level != "dialect")
}

#' Glottolog: remove families
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with glottoget("glottolog")
#' @noRd
#' @family <glottobooster>
#'
glottobooster_rmfamilies <- function(glottologdata){
  glottologdata %>% dplyr::filter(.data$level != "family")
}

#' Glottolog: remove bookkeeping
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with glottoget("glottolog")
#' @noRd
#' @family <glottobooster>
#'
glottobooster_rmbookkeeping <- function(glottologdata){
  # unique(glottologdata$bookkeeping)
  glottologdata %>%
    dplyr::filter(.data$bookkeeping == FALSE) %>%
    dplyr::select(-.data$bookkeeping)
}

#' Glottolog: remove artificial families
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with glottoget("glottolog")
#' @noRd
#' @family <glottobooster>
#'
glottobooster_rmartifam <- function(glottologdata){
  glottologdata %>%
    dplyr::filter(.data$family_id != "arti1236")
}

#' Glottolog: remove sign languages
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with glottoget("glottolog")
#' @noRd
#' @family <glottobooster>
#'
glottobooster_rmsignlangs <- function(glottologdata){
  glottologdata %>%
    dplyr::filter(.data$family_id != "sign1238")
}

#' Glottolog: add family size
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with glottoget("glottolog")
#' @noRd
#' @family <glottobooster>
#'
glottobooster_addfamilysize <- function(glottologdata){
glottologdata %>%
    dplyr::group_by(.data$family_id) %>%
    dplyr::mutate(family_size = dplyr::n())
}

#' Glottolog: add rank order of family size
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with glottoget("glottolog")
#' @noRd
#' @family <glottobooster>
#'
glottobooster_addfamilysizerank <- function(glottologdata){
  glottologdata$family_size_rank <- as.factor(glottologdata$family_size)
  levels(glottologdata$family_size_rank) <- seq(1:length(levels(glottologdata$family_size_rank)))
  glottologdata$family_size_rank  <- as.numeric(glottologdata$family_size_rank) # easier plotting than ordered levels

  glottologdata
}

#' Glottolog: keep only L1 languages
#'
#' Filter out only L1 languages and drop 'category' column.
#'
#' @param glottologdata data from \href{https://glottolog.org/}{glottolog}, can be downloaded with glottoget("glottolog")
#' @noRd
#' @family <glottobooster>
#'
glottobooster_L1only <- function(glottologdata){
  glottologdata %>%
    dplyr::filter(.data$category == "Spoken_L1_Language") %>%
    dplyr::select(-.data$category, -.data$bookkeeping, -.data$level)
}


