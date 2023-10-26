#' Get grambank data
#'
#' This function loads the Glottolog data that is distributed with glottospace or optionally downloads it.
#' type ?glottolog for more information about the version, and how to cite the data. #'
#'
#' @noRd
#'
#' @examples
#' glottoget_glottolog()
glottoget_grambank <- function(download = NULL, dirpath = NULL){
  if(is.null(download)){
    download <- FALSE
  }
  if(download == FALSE & is.null(dirpath) ){
    #out <- glottospace::
    NULL
  } else if(download == FALSE & !is.null(dirpath)){
    out <- glottoget_glottologloadlocal(dirpath = dirpath)
  } else if(download == TRUE){
    out <- glottoget_glottologdownload(dirpath = dirpath)
  }
  return(out)
}
