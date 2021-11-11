
#' Drop glottodata tables and properties
#'
#' @param glottodata glottodata or glottosubdata.
#' @param dropmeta By default all metadata is dropped.
#' @param dropspatial By default spatial properties are dropped.
#' @param dropsub By default, glottosubdata tables are merged into a single glottodata table.
#' @param droplist If glottodata is a list, should the glottodata table be returned?
#' @param tbdf If glottodata is a tibble, should it be converted to a data.frame?
#'
#' @return
#' @export
#'
#' @examples
#' glottodrop(glottodata)
glottodrop <- function(glottodata, dropmeta = TRUE, dropspatial = TRUE, dropsub = TRUE, droplist = TRUE, tbdf = TRUE){

  if(tibble::is_tibble(glottodata) & tbdf == TRUE){
    glottodata <- as.data.frame(glottodata)
  }

  if(glottocheck_hasmeta(glottodata) & dropmeta == TRUE){
    glottodata <- glottodrop_meta(glottodata)
  }

  if(glottocheck_isglottodata(glottodata) & droplist == TRUE){
    glottodata <- glottodrop_list(glottodata)
  }

  if(glottocheck_isglottosubdata(glottodata) & dropsub == TRUE){
    glottodata <- glottojoin_subdata(glottodata)
  }

  if(is_sf(glottodata) & dropspatial == TRUE){
    glottodata <- sf::st_drop_geometry(glottodata)
  }
return(glottodata)
}

#' Drop metdata tables from glottodata
#'
#' @param glottodata Either glottodata or glottosubdata
#'
#' @return
#' @export
#'
glottodrop_meta <- function(glottodata){
  glottodata[!(names(glottodata) %in% c("structure", "metadata",  "references", "readme", "lookup"))]
}

#' Drop language tables from glottodata (keep only meta tables)
#'
#' @param glottodata Either glottodata or glottosubdata
#'
#' @return
#' @export
#'
glottodrop_langtabs <- function(glottodata){
  glottodata[(names(glottodata) %in% c("structure", "metadata",  "references", "readme", "lookup"))]
}

#' Drop list structure of glottodata
#'
#' @param glottodata glottodata
#'
#' @return
#' @export
#'
glottodrop_list <- function(glottodata){
  glottodata$glottodata
}
