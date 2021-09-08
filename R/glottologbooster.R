# # TODO: Match datasources to languages by
# head(glottolog_cldf$sources[,"LGCODE"])

#' Glottologbooster: enhance glottolog data
#'
#' @param glottologdata
#'
#' @return
#' @export
#'
#' @examples
#' glottologdata <- get_glottolog("glottolog")
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

glottolog_addfamilyname <- function(glottologdata){
    families <- glottologdata %>%
      dplyr::filter(level == "family") %>%
      dplyr::transmute(family_id = id, family_name = name)

    dplyr::left_join(x = glottologdata, y = families, by = "family_id")
}

glottolog_addisolates <- function(glottologdata){

    glottologdata$isolate <-   ifelse(
      ( (glottologdata$family_id == "") & (glottologdata$level != "family") ),
      TRUE, FALSE)
    # # set family name to isolate
    glottologdata[glottologdata$isolate == TRUE, "family_name"] <- "isolate"
    # assign glottocode to family_id
    glottologdata[glottologdata$isolate == TRUE, "family_id"] <- glottologdata[glottologdata$isolate == TRUE, "id"]

glottologdata
}

glottolog_rmdialects <- function(glottologdata){
  glottologdata %>% dplyr::filter(level != "dialect")
}

glottolog_rmfamilies <- function(glottologdata){
  glottologdata %>% dplyr::filter(level != "family")
}

glottolog_rmbookkeeping <- function(glottologdata){
  # unique(glottologdata$bookkeeping)
  glottologdata %>%
    dplyr::filter(bookkeeping == "False") %>%
    dplyr::select(-bookkeeping)
}

glottolog_rmartifam <- function(glottologdata){
  glottologdata %>%
    dplyr::filter(family_id != "arti1236")
}

glottolog_rmsignlangs <- function(glottologdata){
  glottologdata %>%
    dplyr::filter(family_id != "sign1238")
}

glottolog_addfamilysize <- function(glottologdata){
glottologdata %>%
    dplyr::group_by(family_id) %>%
    dplyr::mutate(family_size = dplyr::n())

}

#' Create rank order of family size
#'
#' @param glottologdata
#'
#' @return
#' @export
#'
#' @examples
glottolog_addfamilysizerank <- function(glottologdata){
  glottologdata$family_size_rank <- as.factor(glottologdata$family_size)
  levels(glottologdata$family_size_rank) <- seq(1:length(levels(glottologdata$family_size_rank)))
  glottologdata$family_size_rank  <- as.numeric(glottologdata$family_size_rank) # easier plotting than ordered levels

  glottologdata
}


