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
glottoget_dplace <- function(download = NULL, dirpath = NULL){
  if(is.null(download)){
    download <- FALSE
  }
  if(download == FALSE & is.null(dirpath)) {
    out <- glottospace::dplace
  } else if(download == FALSE & !is.null(dirpath)){
    out <- glottoget_dplaceloadlocal(dirpath = dirpath)
  } else if(download == TRUE){
    out <- glottoget_dplacedownload(dirpath = dirpath)
  }
  return(out)
}

#' Download WALS data from zenodo, and select relevant data from cldf data
#'
#' @param dirpath Path to directory where D-PLACE cldf data is stored
#'
#' @noRd
#'
glottoget_dplacedownload <- function(dirpath){
  invisible(readline(prompt="Are you sure you want to download DPLACE data? \n Press [enter] to continue"))
  dirpath <- glottoget_zenodo(name = "dplace", dirpath = dirpath)
  glottoget_dplaceloadlocal(dirpath = dirpath, valuenames = valuenames, paramnames = paramnames)
}

#' Load locally stored WALS data (without joining with glottolog)
#'
#' @param dirpath Path to directory where D-PLACE cldf data is stored
#'
#' @noRd
glottoget_dplaceloadlocal <- function(dirpath){
  if(!dir.exists(dirpath)){stop("Directory not found.")}
  if(is.null(valuenames)){valuenames <- TRUE}
  if(is.null(paramnames)){paramnames <- FALSE}

  cldf_metadata <- base::list.files(dirpath, pattern = "-metadata.json", recursive = TRUE)
  mdpath <- normalizePath(file.path(dirpath, cldf_metadata))
  mddir <- normalizePath(base::dirname(mdpath))

  # Load languages file
  languoids <- normalizePath(file.path(mddir, "languages.csv"))
  languoids <- utils::read.csv(languoids, header = TRUE, encoding = "UTF-8")
  colnames(languoids) <- base::tolower(colnames(languoids))
  colnames(languoids)[which(colnames(languoids) == "id")] <- "lang_id"


  # Load values file
  values <- normalizePath(file.path(mddir, "values.csv"))
  values <- utils::read.csv(values, header = TRUE, encoding = "UTF-8")
  colnames(values) <- base::tolower(colnames(values))
  colnames(values)[colnames(values) == "language_id"] <- "lang_id"
  params <- unique(values$parameter_id)
  if(valuenames == FALSE){
    values <- tidyr::pivot_wider(data = values, names_from = "parameter_id", values_from = "value")
  } else {    # Join values to codes by code_id
    codes <- normalizePath(file.path(mddir, "codes.csv"))
    codes <- utils::read.csv(codes, header = TRUE, encoding = "UTF-8")
    values <- values %>% dplyr::left_join(codes, by = c("code_id" = "ID") )
    values <- tidyr::pivot_wider(data = values, names_from = "parameter_id", values_from = "Name")
  }

  # Create empty data frame to store results
  langs <- unique(values$lang_id)
  langvals <- data.frame(matrix(ncol = ncol(values), nrow = length(langs)))
  colnames(langvals) <- colnames(values)
  langvals[,"lang_id"] <- langs

  for(i in seq_along(langs)){
    lang <- langs[[i]]
    langtb <- values[values[,"lang_id"] == lang, params]
    langvals[langvals["lang_id"] == lang, params] <- apply(X = langtb, MARGIN = 2, FUN = nonna, max1 = TRUE)
  }

  walsdata <- languoids %>% dplyr::left_join(langvals, by = "lang_id")
  walsdata <- base::subset(walsdata, select = c("glottocode", params))
  walsdata <- walsdata[!purrr::is_empty(walsdata$glottocode) & walsdata$glottocode != "", ]

  if(paramnames == TRUE){# Add parameter labels
    parameters <- normalizePath(file.path(mddir, "parameters.csv"))
    parameters <- utils::read.csv(parameters, header = TRUE, encoding = "UTF-8")
    colnames(walsdata)[-1] <- parameters$Name[match(colnames(walsdata), parameters$ID )][-1]
  }

  walsdata <- glottojoin_base(walsdata)
  invisible(walsdata)
  # EApath <- "D:/Linguistic/D-PLACE/dplace-data-v2.2.0/D-PLACE-dplace-data-541f9f8/datasets/EA/"
  # https://zenodo.org/record/5554412
  #
  # # variables <- read.csv(paste0(EApath, "variables.csv"))
  # eadata <- read.csv(paste0(EApath, "data.csv"))
  # eadata <- eadata %>% select(soc_id, var_id, code)
  # eadata <-  tidyr::spread(data = eadata, key = "var_id", value = "code")
  # eatb <- as_tibble(eadata)
  #
  # socdf <- read.csv(paste0(EApath, "societies.csv"))
  # soctb <- as_tibble(socdf)
  #
  # dplace <- left_join(soctb, eatb, by = c("id" = "soc_id"))
  # # gcea <- c("glottocode", "id", grep(colnames(dplace), pattern = "EA", value = TRUE) )
  # #
  # # dplace <- dplace[, gcea] %>% rename(soc_id = id)
  #
  # gcea <- c("glottocode", grep(colnames(dplace), pattern = "EA", value = TRUE) )
  #
  # dplace <- dplace[, gcea]
  #
  # write.xlsx(dplace, 'data/dplace_ea.xlsx')

}


