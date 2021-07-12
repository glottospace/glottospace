

#' Download glottolog data and convert it into a glot or geoglot object
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
  strex::match_arg(data, c("glottolog", "glottolog_spatial",
                           "glottolog_cldf", "glottolog_cldf_spatial"), ignore_case = TRUE)
  if(data == "glottolog"){
    glottodata <- glottolog_download()
  } else if(data == "glottolog_spatial"){
    d <- glottolog_download()
    glottodata <- glottodata_spatial(glottodata = d)
  } else if(data == "glottolog_cldf"){
    cldfpath <- glottolog_download_cldf()
    glottodata <- read_cldf(mdpath = cldfpath)
  } else if(data == "glottolog_cldf_spatial"){
    cldfpath <- glottolog_download_cldf()
    glottodata <- read_cldf(mdpath = cldfpath)
    glottodata$tables$languages <- glottodata_spatial(glottodata = data$tables$languages,
                                                lon = "Longitude", lat = "Latitude")
  # } else if(tools::file_ext(data) == "csv"){
  #   data <- utils::read.csv(data, header = TRUE)
  # } else if(class(try(httr::GET(data), TRUE)) == "response"){
  #   urltry <- try(httr::GET(data), TRUE)
  #   isTRUE(class(urltry)=="try-error")
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

glottodata_spatial <- function(glottodata = NULL, lon = "longitude", lat = "latitude"){
  if(is.null(glottodata)){
    glottologdata <- glottolog_download()
    cat("No input data provided, glottolog data downloaded")
  }
  glottolatlon <- glottodata %>%
    dplyr::filter(!is.na(!!as.symbol(lon))) %>%
    dplyr::filter(!is.na(!!as.symbol(lat)))

    glottodata <- sf::st_as_sf(x = as.data.frame(glottolatlon),
                     coords = c(lon, lat),
                     crs = 4326) #https://epsg.io/4326

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

cldf_md <- base::list.files(destdir, pattern = "cldf-metadata.json", recursive = TRUE)
cldfpath <- paste(destdir,
                   stringr::str_remove(cldf_md, "cldf-metadata.json"), sep = "/" )
return(cldfpath)
}

#' List names of all sheets in an excel file (.xls or .xlsx). This is a wrapper around excel_sheets.
#' Use the function \code{"get_sheetdata"}  to import data.
#'
#' @param path Path to excel file
#'
#' @return Vector of names
#' @export
#'
#' @examples
#' get_sheetnames(path = "glottodata.xlsx")
#' get_sheetnames(path = "glottodata.xls")
get_sheetnames <- function(path){
  sheetnames <- readxl::excel_sheets(path)
  message("Excel file contains the following sheets: \n")
  message(paste(sheetnames, "\n"))
  message('Use the function get_sheetdata to import data')
  sheetnames
}

#' Import sheets from an excel file (.xls or .xlsx). This is a wrapper around excel_sheets.
#' Use the function \code{"get_sheetnames"} to show all sheetnames in an excel file (without importing them).
#'
#' @param sheets Character vector of sheetnames to import. Leave empty in case you want to import all sheets.
#' @param path Path to excel file (.xls or .xlsx)
#' @param simplify If only one sheet is selected, should the data be returned without placing it inside a list (which would be a list of length one)?
#'
#' @return List with tibble per sheet
#' @export
#'
#' @examples
#' # 1. There is one sheet in which each row represents a glot (anything with a glottocode). The first column contains the glottocodes.
#' get_sheetdata(path = "glottodata.xlsx")
#' get_sheetdata(path = "glottodata.xlsx", sheets = "glottodata")
#' # 2. There are several sheets, each of which represents a glot. The name of the sheets are glottocodes.  The first column in each sheet contains glottosubcodes (abcd1234_a1)
#' get_sheetdata(path = "glottodata.xlsx", sheets = c("abcd1234", "abcd1235", "abcd1236") )
#' # 3. There are several sheets, each of which represents a subglot. The name of the sheets are glottosubcodes. The first column in each sheet contains glottosubsubcodes (abcd1234_a1_a1)
#' get_sheetdata(path = "glottodata.xlsx", sheets = c("abcd1234_form", "abcd1234_meaning", "abcd1235_form", "abcdf1235_meaning") )

get_sheetdata <- function(path, sheets = NULL, simplify = TRUE){
  if(is.null(sheets)){
    data <- base::lapply(X = readxl::excel_sheets(path),
                                     FUN = readxl::read_excel, path = path)
    } else {
    data <- base::lapply(X = sheets, FUN = readxl::read_excel, path = path)}
  if(length(sheets) == 1 & simplify == TRUE){
    data <- readxl::read_xlsx(path = path, sheet = sheets)
  }
  return(data)
}


# d1 <- "C:/Users/sjnor/surfdrive/Shared/SAPPHIRE/Output/Presentations/Isolates_WS-Athens/data/dbase_isolates_V2.xlsx"
# d2 <- "C:/Users/sjnor/surfdrive/Shared/Tanimuka_Yucuna/Databases/Voor_Sietze/NounPhrase_V6.xlsx"
# d3 <- "C:/Users/sjnor/surfdrive/Shared/Tanimuka_Yucuna/Databases/Voor_Sietze/TAME_V6.xlsx.xlsx"


# CHECK WITHIN ONE ELEMENT OF A LIST
glottodatacheck <- function(){
  # Check levels
  levl <- lapply(data[,colnames(data)!= id], unique)
  # levels <- unlist(lapply(levl, paste, collapse=" , "))
  cat('The variables have the following levels: \n')
  # print(as.data.frame(levels))
  print(levl)

  # data <- as_tibble(data)
  # print(data)

  # Check missing IDs
  idmissing <- nrow(data[is.na(data[,id]),] )
  if(idmissing > 0){
    message(paste(idmissing, ' rows with missing ID'))
  } else{message("No missing IDs")}

  # Check whether ids are unique
  freqtab <- data.frame(table(data[,id]))
  colnames(freqtab)[1] <- "id"
  colnames(freqtab)[2] <- "n"

  if(any(freqtab$n > 1)){
    duplicate <- freqtab[freqtab$n > 1, ]
    message('IDs are not unique. The following ids have duplicates:')
    print(duplicate)
  } else{message("No duplicate IDs.")}

  if(printlevels == TRUE){
    # Print levels of all columns
    levl <- lapply(data, unique)
    levels <- unlist(lapply(levl, paste, collapse=" , "))
    cat('The variables have the following levels: \n')
    print(as.data.frame(levels))
  }

}
# glottodatacheck
# gd_check

# CHECK ACROSS ALL ELEMENTS OF A LIST
glottodatacheck_colcount(){
  # Check whether number of columns are identical across languages
colcount <- lapply(X = lsls, FUN = function(x){length(colnames(x))})
colcount <- unlist(colcount, recursive = F)
if(length(unique(colcount)) > 1){
  message('Not all languages have same number of features \n')
  message(paste(glottocodes, ": ", colcount, "\n"))
}

}

# Check metasheet: weight, type, colnames
