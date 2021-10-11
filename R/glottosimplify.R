
#' Simplify glottodata
#'
#' @param glottodata
#'
#' @return
#' @export
#'
#' @examples
#' glottodrop(glottodata)
glottodrop <- function(glottodata){

if(glottocheck_hasmeta(glottodata)){


  # if(simple == TRUE & length(glottodata) == 1 & any(class(glottodata) == "list") ){
  #   glottodata <- glottodata[[1]]
  # }
}

if(glottocheck_isglottosubdata(glottodata)){
  glottodata <- join_glottodatalist(glottodata)
}

if(is_sf(glottodata)){
  glottodata <- sf::st_drop_geometry(glottodata)
}
}
