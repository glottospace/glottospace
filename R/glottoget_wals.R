#' Get WALS data
#'
#' This function loads the WALS data that is distributed with glottospace or optionally downloads it.
#' type ?wals for more information about the version, and how to cite the data.
#'
#' @param days After how many days should be checked for a new version?
#'
#' @noRd
#' @examples
#' \donttest{
#' glottoget_wals()
#' }
glottoget_wals <- function(download = NULL, dirpath = NULL){
  if(is.null(download)){
    download <- FALSE
    }
  if(download == FALSE & is.null(dirpath)) {
    out <- glottospace::wals
  } else if(download == FALSE & !is.null(dirpath)){
    out <- glottoget_cldf(dirpath = dirpath, name = "wals")
  } else if(download == TRUE){
    out <- glottoget_walsdownload(dirpath = dirpath)
  }
  return(out)
}

#' Download WALS data from zenodo, and select relevant data from cldf data
#'
#'
#'
#' @noRd
#'
glottoget_walsdownload <- function(dirpath = NULL){
  invisible(readline(prompt="Are you sure you want to download WALS data? \n Press [enter] to continue"))
  dirpath <- glottoget_zenodo(name = "wals", dirpath = dirpath)
  glottoget_cldf(dirpath = dirpath, name = "wals", valuenames = TRUE)
}
