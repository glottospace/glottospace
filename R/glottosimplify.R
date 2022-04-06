
#' Simplify glottodata structures
#'
#' With glottosimplify, the structure of a glottodata object is simplified by removing tables and properties
#'
#' @param glottodata glottodata or glottosubdata.
#' @param dropmeta By default all metadata is removed.
#' @param dropspatial By default spatial properties are removed.
#' @param submerge By default, glottosubdata tables are merged into a single glottodata table.
#' @param droplist By default, if only one sheet is loaded, the data will be returned as a data.frame (instead of placing the data inside a list of length 1)
#' @param dropunits By default units are kept.
#' @return a simplified version of the original dataset, either a data.frame/tibble or a list (depending on the selected options)
#'
#' @export
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottosimplify(glottodata)
glottosimplify <- function(glottodata, droplist = TRUE, dropmeta = TRUE, dropspatial = TRUE, submerge = TRUE, dropunits = FALSE){

  glottodata <- contrans_tb2df(glottodata)

  if(dropmeta == TRUE){
    glottodata <- glottosimplify_dropmeta(glottodata)
  }

  if(submerge == TRUE){
    glottodata <- glottosimplify_submerge(glottodata)
  }

  if(droplist == TRUE){
    glottodata <- glottosimplify_droplist(glottodata)
  }

  if(dropspatial == TRUE){
    glottodata <- glottosimplify_dropspatial(glottodata)
  }

  if(dropunits == TRUE){
    glottodata <- glottosimplify_dropunits(glottodata)
  }

  return(glottodata)
}

#' Drop metadata tables from glottodata
#'
#' @param glottodata Either glottodata or glottosubdata
#' @noRd
#'
glottosimplify_dropmeta <- function(glottodata){
  if(glottocheck_hasmeta(glottodata)){
    glottodata[!(names(glottodata) %in% names(glottocreate_metadatatables()))]
  } else {
    glottodata
  }
}

#' Drop language tables from glottodata (keep only meta tables)
#'
#' @param glottodata Either glottodata or glottosubdata
#' @noRd
#'
glottosimplify_langtabs <- function(glottodata){
  glottodata[(names(glottodata) %in% c("structure", "metadata",  "references", "readme", "lookup"))]
}

#' Drop list structure of glottodata
#'
#' Select glottodata table or glottosubdata table from a glottodatalist
#'
#' @param glottodata glottodatalist
#' @noRd
#'
glottosimplify_droplist <- function(glottodata){
  if(glottocheck_isglottodata(glottodata) | glottocheck_isglottosubdata(glottodata)){
    if(length(glottodata) == 1 & inherits(glottodata, what = "list" ) ){
      glottodata <- glottodata[names(glottodata) %in% c("glottodata", "glottosubdata")]
      glottodata <- glottodata[[1]]
    }
  }
  return(glottodata)
}

#' Drop spatial properties from glottodata
#'
#' @param glottodata Either glottodata or glottosubdata
#' @noRd
#'
glottosimplify_dropspatial <- function(glottodata){
  if(is_sf(glottodata) ){
    sf::st_drop_geometry(glottodata)
  } else {
    glottodata
  }
}

#' Drop units from glottodata
#'
#' @param glottodata Either glottodata or glottosubdata
#' @noRd
#'
glottosimplify_dropunits <- function(glottodata){
  units::drop_units(glottodata)
}


#' Conditionally merge tables from glottosubdata
#'
#' Check whether input is glottosubdata and merge tables if TRUE.
#'
#' @param glottosubdata glottosubdata.
#' @noRd
#'
glottosimplify_submerge <- function(glottosubdata){
  if(glottocheck_isglottosubdata(glottosubdata) ){
    glottojoin_subdata(glottosubdata)
  } else {
    glottosubdata
  }
}
