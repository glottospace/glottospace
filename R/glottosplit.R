
#' Split or merge metadata from glottodata (or glottosubdata)
#'
#' Usually, you will run this function twice, once to split metadata from glottodata, and a second time to join it again.
#'
#' @param glottodata glottodata
#' @param splitted if provided, the second element of the list will be joined with glottodata
#'
#' @return A list of length 2 in case only glottodata is provided, and a merged glottodata object otherwise.
#' @export
#' @seealso glottojoin
#' @seealso glottosimplify
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' splitted <- glottosplitmergemeta(glottodata)
#' merged <- glottosplitmergemeta(glottodata = glottodata, splitted = splitted)
glottosplitmergemeta <- function(glottodata, splitted = NULL){
  if(is.null(splitted)){
    splitted <- glottosplit(glottodata)
    return(splitted)
  } else {
    if(any(!is.na(splitted[[2]]))){
      glottojoin(glottodata = glottodata, with = splitted[[2]])
    } else {
      glottodata
    }
  }

}

#' Split metadata and data from glottodata or glottosubdata
#'
#' Splits data and metadata from glottodata or glottosubdata and returns a list of two (1: glottodata tables, and 2: metadata tables )
#'
#' If data does not contain metadata, the second list element will be NA.
#'
#' @param glottodata glottodata or glottosubdata (either with or without metadata)
#' @return A list of length 2
#' @noRd
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' splitted <- glottosplit(glottodata)
#' merged <- glottojoin(splitted[[1]], splitted[[2]])
glottosplit <- function(glottodata){

  glottosplit <- vector(mode = "list", length = 2)


  if(glottocheck_isglottodata(glottodata)){
    names(glottosplit) <- c("glottodata", "metadata")
      if(glottocheck_hasmeta(glottodata) ){
        glottosplit[[1]] <- glottodata[["glottodata"]]
        glottosplit[[2]] <- glottodata[names(glottodata) != "glottodata"]
      } else {
        glottosplit[[1]] <- glottosimplify(glottodata, dropspatial = FALSE)
        glottosplit[[2]] <- NA
      }
  } else if (glottocheck_isglottosubdata(glottodata)){
    names(glottosplit) <- c("glottosubdata", "metadata")
    metatables <- names(glottodata) %in% names(glottocreate_metadatatables())
    if(glottocheck_hasmeta(glottodata) ){
      if(sum(!metatables) == 1 & all(names(glottodata)[!metatables] == "glottosubdata")){
        glottosplit[[1]] <- glottodata[["glottosubdata"]]
        glottosplit[[2]] <- glottodata[metatables]
      } else{
        glottosplit[[1]] <- glottodata[!metatables]
        glottosplit[[2]] <- glottodata[metatables]
      }
    } else {
        glottosplit[[1]] <- glottodata[!metatables]
        glottosplit[[2]] <- NA
    }
  }
return(glottosplit)

}
