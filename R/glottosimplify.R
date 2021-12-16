
#' Simplify glottodata structures
#'
#' With glottosimplify, the structure of a glottodata object is simplified by removing tables and properties
#'
#' @param glottodata glottodata or glottosubdata.
#' @param dropmeta By default all metadata is removed.
#' @param dropspatial By default spatial properties are removed.
#' @param submerge By default, glottosubdata tables are merged into a single glottodata table.
#' @param list2tab By default if glottodata is a list of tables, only the glottodata table is returned.
#'
#' @return
#' @export
#'
#' @examples
#' glottosimplify(glottodata)
glottosimplify <- function(glottodata, dropmeta = TRUE, dropspatial = TRUE, submerge = TRUE, list2tab = TRUE){

  glottodata <- contrans_tb2df(glottodata)


  if(dropmeta == TRUE){
    glottodata <- glottosimplify_dropmeta(glottodata)
  }

  if(list2tab == TRUE){
    glottodata <- glottosimplify_list2tab(glottodata)
  }

  if(submerge == TRUE){
    glottodata <- glottosimplify_submerge(glottodata)
  }

  if(dropspatial == TRUE){
    glottodata <- glottosimplify_dropspatial(glottodata)
  }
  return(glottodata)
}

#' Drop metadata tables from glottodata
#'
#' @param glottodata Either glottodata or glottosubdata
#'
#' @return
#' @export
#'
glottosimplify_dropmeta <- function(glottodata){
  if(glottocheck_hasmeta(glottodata)){
    glottodata[!(names(glottodata) %in% c("structure", "metadata",  "references", "readme", "lookup"))]
  } else {
    glottodata
  }
}

#' Drop language tables from glottodata (keep only meta tables)
#'
#' @param glottodata Either glottodata or glottosubdata
#'
#' @return
#' @export
#'
glottosimplify_langtabs <- function(glottodata){
  glottodata[(names(glottodata) %in% c("structure", "metadata",  "references", "readme", "lookup"))]
}

#' Select glottodatatable from a glottodatalist
#'
#' @param glottodata glottodatalist
#'
#' @return
#' @export
#'
glottosimplify_list2tab <- function(glottodata){
  if(glottocheck_isglottodata(glottodata) & any(class(glottodata) == "list" )){
    return(glottodata$glottodata)
    } else {
    return(glottodata)
  }
}

#' Drop spatial properties from glottodata
#'
#' @param glottodata Either glottodata or glottosubdata
#'
#' @return
#' @export
#'
glottosimplify_dropspatial <- function(glottodata){
  if(is_sf(glottodata) ){
    sf::st_drop_geometry(glottodata)
  } else {
    glottodata
  }
}

#' Conditionally merge tables from glottosubdata
#'
#' Check whether input is glottosubdata and merge tables if TRUE.
#'
#' @param glottosubdata glottosubdata.
#'
#' @return
#' @export
#'
glottosimplify_submerge <- function(glottosubdata){
  if(glottocheck_isglottosubdata(glottosubdata) ){
    glottojoin_subdata(glottosubdata)
  } else {
    glottosubdata
  }
}
