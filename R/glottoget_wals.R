#' Get WALS data
#'
#' This function checks whether most recent version of WALS is locally available. If local version is outdated, the newest version will be downloaded.
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
glottoget_walsdownload <- function(dirpath, valuenames = NULL, paramnames = NULL){
  invisible(readline(prompt="Are you sure you want to download WALS data? \n Press [enter] to continue"))
  dirpath <- glottoget_zenodo(name = "wals", dirpath = dirpath)
  glottoget_walsloadlocal(dirpath = dirpath, valuenames = valuenames, paramnames = paramnames)
}

#' Load locally stored WALS data (without joining with glottolog)
#'
#' @param valuenames Should names of the values be added instead of codes?
#' @param paramnames Should names of parameters (columns) be added instead of their codes?
#'
#'
#' @noRd
glottoget_walsloadlocal <- function(dirpath, valuenames = NULL, paramnames = NULL){
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
}


