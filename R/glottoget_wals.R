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

#' Add codes to WALS
#'
#' @param dirpath Path to directory where cldf data is stored
#' @param name Name of a dataset, either glottolog, wals or dplace
#'
#' @noRd
glottoget_addwalscodes <- function(wals, dirpath, name){
  if(!dir.exists(dirpath)){stop("Directory not found.")}

  cldf_metadata <- base::list.files(dirpath, pattern = "-metadata.json", recursive = TRUE)
  cldfid <- grep(pattern = tolower(name), x = tolower(cldf_metadata))
  mdpath <- normalizePath(file.path(dirpath, cldf_metadata[[cldfid]]))
  mddir <- normalizePath(base::dirname(mdpath))

  # Load codes file
  codes <- normalizePath(file.path(mddir, "codes.csv"))
  codes <- utils::read.csv(codes, header = TRUE, encoding = "UTF-8")

  ws <- wals[, colnames(wals) == "143A"]
  colnames(ws)[1] <- "Number"
  wc <- codes[codes$Parameter_ID == "143A",]
  dplyr::left_join(ws, wc, by = "Number")

  # new <- sf::st_drop_geometry(ws)
  # new[] <- wc$Name[match(unlist(ws), wc$Number)]

  # wals %>%
  #   tidyr::pivot_longer(cols = `143A`) %>%
  #   dplyr::left_join(codes, by = "Number") %>%
  #   tidyr::spread(key = Number, value = Name)

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
  glottoget_cldf(dirpath = dirpath, name = "wals")
}
