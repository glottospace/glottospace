

#' Download glottolog data
#'
#' @param data One of \code{"glottolog"}, \code{"glottolog_spatial"}, or
#'   \code{"glottolog_cldf"} or \code{"glottolog_cldf_spatial"}. By default, the downloaded glottolog data is
#'   converted into a geoglot object (languages without coordinates are removed).
#'
#' @return Either a glot or geoglot object.
#' @export
#'
#' @examples
#' glottodata <- get_glottolog()
get_glottolog <- function(data = "glottolog_spatial"){
  # TODO: if download fails, load inbuilt glottobase (which won't be the latest version of glottolog)

  strex::match_arg(data, c("glottolog", "glottolog_cldf"), ignore_case = TRUE)
  if(data == "glottolog"){
    glottodata <- glottolog_download()
  } else if(data == "glottolog_cldf"){
    cldfpath <- glottolog_download_cldf()
    glottodata <- read_cldf(mdpath = cldfpath)
  } else {
    stop("Could not get glottolog data")
  }
  return(glottodata)
}

glottolog_download <- function(){
  # FIXME: Now URL is fixed, which means that it doesn't update when newer version of glottolog becomes available.
  # TODO: try other URL if first one fails.
  # https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
  # from glottolog website or from zenodo.

  # https://github.com/cran/raster/blob/master/R/getData.R
  # https://rdrr.io/github/inbo/inborutils/src/R/download_zenodo.R
  base_url <- "https://cdstar.shh.mpg.de/bitstreams/EAEA0-E62D-ED67-FD05-0/"
  filename <- "glottolog_languoid.csv.zip"
  url <- paste0(base_url, filename)
  if(!base::file.exists(filename)){
  utils::download.file(url = url, destfile = filename)}
  data <- utils::read.csv(unz(filename, "languoid.csv"), header = TRUE)
}



glottolog_download_cldf <- function(destdir = tempdir()){
base_url <-  "https://zenodo.org/api/records/4762034"
req <- curl::curl_fetch_memory(base_url)
content <- RJSONIO::fromJSON(rawToChar(req$content))
# title <- gsub(".*:", "", content$metadata$title)
# version <- content$metadata$version
url <- content$files[[1]]$links[[1]]
# filename <- base::basename(url)
destdir <- paste0(normalizePath(destdir, winslash = "/", mustWork = FALSE), "/cldf")
tmpfile <- tempfile()
# tmpfile <- tempfile(tmpdir = destdir)
utils::download.file(url = url, destfile = tmpfile )
utils::unzip(zipfile = tmpfile, exdir = destdir)
# FIXME: regex *-metadata.json
cldf_md <- base::list.files(destdir, pattern = "cldf-metadata.json", recursive = TRUE)
cldfpath <- paste(destdir,
                   stringr::str_remove(cldf_md, "cldf-metadata.json"), sep = "/" )
return(cldfpath)
}

