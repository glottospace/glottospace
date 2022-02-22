#' Get WALS data
#'
#' This function loads the WALS data that is distributed with glottospace or optionally downloads it.
#' type ?wals for more information about the version, and how to cite the data.
#'
#' @param days After how many days should be checked for a new version?
#' @param valuenames Should names of the values be added instead of codes?
#' @param paramnames Should names of parameters (columns) be added instead of their codes?
#'
#' @noRd
#' @examples
#' \donttest{
#' glottoget_wals()
#' }
glottoget_wals <- function(download = NULL, dirpath = NULL, valuenames = NULL, paramnames = NULL){
  if(is.null(download)){
    download <- FALSE
    }
  if(download == FALSE & is.null(dirpath)) {
    out <- glottospace::wals
  } else if(download == FALSE & !is.null(dirpath)){
    out <- glottoget_walsloadlocal(dirpath = dirpath, valuenames = valuenames, paramnames = paramnames)
  } else if(download == TRUE){
    out <- glottoget_walsdownload(dirpath = dirpath, valuenames = valuenames, paramnames = paramnames)
  }
  return(out)
}

#' Download WALS data from zenodo, and select relevant data from cldf data
#'
#' @param valuenames Should names of the values be added instead of codes?
#' @param paramnames Should names of parameters (columns) be added instead of their codes?
#'
#'
#' @noRd
#'
glottoget_walsdownload <- function(dirpath = NULL, valuenames = NULL, paramnames = NULL){
  invisible(readline(prompt="Are you sure you want to download WALS data? \n Press [enter] to continue"))
  dirpath <- glottoget_zenodo(name = "wals", dirpath = dirpath)
  glottoget_cldfloadlocal(dirpath = dirpath, valuenames = valuenames, paramnames = paramnames)
}
