#' Get WALS data
#'
#' This function checks whether most recent version of WALS is locally available. If local version is outdated, the newest version will be downloaded.
#'
#' @return
#' @export
#'
#' @examples
#' glottoget_wals()
glottoget_wals <- function(days = NULL){
  if(is.null(days)){days <- 30}
  if(curl::has_internet() & wals_date_local() < (-days) ){
    message(paste("Your local version of WALS was downloaded more than ", days, " days ago."))
    vremote <- wals_version_remote()
    vlocal <- wals_version_local()
    if(vremote == vlocal){
      out <- wals_loadlocal()
      message(paste("WALS is up-to-date. Version", vlocal, " loaded."))
    } else if(vremote > vlocal){
      out <- wals_download()
    }
  } else { # Try to load local data, or else load built-in data.
    out <- try(
      expr = wals_loadlocal(),
      silent = TRUE
    )
    if(any(class(out) == "try-error")){
      data("wals")
      out <- wals
    }

  }
  return(out)
}

#' Check how long ago WALS data was downloaded
#'
#' @return Number of days passed since WALS data was downloaded for the last time
#' @export
#'
wals_date_local <- function(){
  v <- wals_version_local()
  if(v != 0){
    newestdir <- glottofiles_makepath(paste0("wals-v", v))
    wals_time <- file.info(newestdir)$ctime
    daysago <- lubridate::as.duration(lubridate::interval(Sys.time(), wals_time)) %/% lubridate::as.duration(lubridate::days(1))
    return(daysago)
  } else{
    return(-999999)
  }
}

#' Check what's the most recent version of WALS
#'
#' @return
#' @export
#'
wals_version_remote <- function(){
  base_url <-  "https://zenodo.org/api/records/596476"
  message("Checking what's the most recent version of WALS ... this might take a while")
  req <- curl::curl_fetch_memory(base_url)
  content <- RJSONIO::fromJSON(rawToChar(req$content))
  # title <- gsub(".*:", "", content$metadata$title)
  as.numeric(gsub(pattern = "v", x = content$metadata$version, replacement = ""))
}

#' Download WALS data from zenodo, and select relevant data from cldf data
#'
#' @return
#' @export
#'
wals_download_cldf <- function(){
  wals_download_zenodo()
  wals_loadlocal()
}

#' Download most recent version of WALS from zenodo (cldf format)
#'
#' @return
#' @export
#'
wals_download_zenodo <- function(){
  base_url <-  "https://zenodo.org/api/records/596476" # Newest version is always uploaded here!
  req <- curl::curl_fetch_memory(base_url)
  content <- RJSONIO::fromJSON(rawToChar(req$content))
  url <- content$files[[1]]$links[[1]]
  filename <- base::basename(url)
  filepath <- glottofiles_makepath(filename)
  exdir <- glottofiles_makedir(tools::file_path_sans_ext(filename))

  utils::download.file(url = url, destfile = filepath ) # downloads and overwrites (i.e. changes date)
  utils::unzip(zipfile = filepath, exdir = exdir)
  message(paste0("WALS data downloaded (wals-", content$metadata$version,"). This is the most recent version available from https://zenodo.org/record/596476)") )
}

#' Check which version of WALS is available on your computer
#'
#' @return
#' @export
#'
wals_version_local <- function(){
  files <- base::list.files(glottofiles_cachedir(), full.names = FALSE, recursive = FALSE)
  if(purrr::is_empty(files)){
    return(0)
  } else{
    walsfiles <- files[base::grepl(pattern = "wals-v", x = files)]
    walszips <- walsfiles[grepl(pattern = ".zip", x = walsfiles)]
    if(purrr::is_empty(walszips)){
      return(0)
    } else{
      versionzips <- base::gsub(pattern = "wals-v", x = walszips, replacement = "")
      versions <- tools::file_path_sans_ext(versionzips)
      return(max(as.numeric(versions)))
    }
  }

}

wals_loadlocal <- function(){
  walsdata <- wals_loadlocal_raw()
  glottojoin_base(walsdata)
}

#' Load locally stored WALS data (without joining with glottolog)
#'
#' @return
#' @export
#'
wals_loadlocal_raw <- function(){
  exdir <- glottofiles_makedir(paste0("wals-v", wals_version_local()))
  cldf_metadata <- base::list.files(exdir, pattern = "-metadata.json", recursive = TRUE)
  mdpath <- normalizePath(file.path(exdir, cldf_metadata))
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
  values <- tidyr::pivot_wider(data = values, names_from = "parameter_id", values_from = "value")

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
  invisible(walsdata)
}