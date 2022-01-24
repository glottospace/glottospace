#' Split metadata and data from glottodata or glottosubdata
#'
#' Splits data and metadata from glottodata or glottosubdata and returns a list of two (1: glottodata tables, and 2: metadata tables )
#'
#' If data does not contain metadata, the second list element will be NA.
#'
#' @param glottodata
#'
#' @return A list of length 2
#' @export
#' @seealso glottojoin
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' splitted <- glottosplit(glottodata)
#' merged <- glottojoin(splitted[[1]], splitted[[2]])
glottosplit <- function(glottodata){

  glottosplit <- vector(mode = "list", length = 2)
  names(glottosplit) <- c("glottodata", "metadata")

  if(glottocheck_isglottodata(glottodata)){
      if(glottocheck_hasmeta(glottodata) ){
        glottosplit[[1]] <- glottodata[["glottodata"]]
        glottosplit[[2]] <- glottodata[names(glottodata) != "glottodata"]
      } else {
        glottosplit[[1]] <- glottosimplify(glottodata)
        glottosplit[[2]] <- NA
      }
  } else if (glottocheck_isglottosubdata(glottodata)){
    metatables <- names(glottodata) %in% names(glottoget("demodata", meta = TRUE))[-1]
    if(glottocheck_hasmeta(glottodata) ){
        glottosplit[[1]] <- glottodata[!metatables]
        glottosplit[[2]] <- glottodata[metatables]
    } else {
        glottosplit[[1]] <- glottodata[!metatables]
        glottosplit[[2]] <- NA
    }
  }
return(glottosplit)

}
