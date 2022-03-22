#' Get DPLACE data
#'
#' This function loads the DPLACE data that is distributed with glottospace or optionally downloads it.
#' type ?dplace for more information about the version, and how to cite the data.
#'
#' @param dirpath Path to directory where D-PLACE cldf data is stored
#'
#' @noRd
#' @examples
#' \donttest{
#' glottoget_dplace()
#' }
glottoget_dplace <- function(download = NULL, dirpath = NULL, valuenames = NULL, paramnames = NULL){
  if(is.null(download)){
    download <- FALSE
  }
  if(download == FALSE & is.null(dirpath)) {
    out <- glottospace::dplace
  } else if(download == FALSE & !is.null(dirpath)){
    out <- glottoget_cldf(dirpath = dirpath, name = "dplace")
  } else if(download == TRUE){
    out <- glottoget_dplacedownload(dirpath = dirpath)
  }
  return(out)
}

#' Download D-PLACE data from zenodo, and select relevant data from cldf data
#'
#' @param dirpath Path to directory where D-PLACE cldf data is stored
#'
#' @noRd
#'
glottoget_dplacedownload <- function(dirpath = NULL, valuenames = NULL, paramnames = NULL){
  invisible(readline(prompt="Are you sure you want to download DPLACE data? \n Press [enter] to continue"))
  dirpath <- glottoget_zenodo(name = "dplace", dirpath = dirpath)
  glottoget_cldf(dirpath = dirpath, name = "dplace")
}



