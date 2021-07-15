#' List names of all sheets in an excel file
#'
#' Reads an excel file (.xls or .xlsx).
#' Use the function \code{"get_sheetdata"}  to import data.
#'
#' This is a wrapper around excel_sheets.
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
  message("Excel file contains the following sheets (use the function get_sheetdata for import): \n")
  sheetnames
}

#' Import sheets from an excel file
#'
#' Import sheets from an excel file (.xls or .xlsx).
#' Use the function \code{"get_sheetnames"} to show all sheetnames in an excel file (without importing them).
#'
#' This is a wrapper around excel_sheets.
#'
#' @param sheets Character vector of sheetnames to import. Leave empty in case you want to import all sheets.
#' @param path Path to excel file (.xls or .xlsx)
#' @param simplify If TRUE and only one sheet is selected, the data will be returned without placing it inside a list (which would be a list of length one).
#'
#' @return Named list with one table (tibble) per glot.
#' @export
#'
#' @examples
#' # 1. There is one sheet in which each row represents a glot (anything with a glottocode). The first column contains the glottocodes.
#' get_sheetdata(path = "glottodata.xlsx")
#' get_sheetdata(path = "glottodata.xlsx", sheets = 1)
#' get_sheetdata(path = "glottodata.xlsx", sheets = "glottodata")
#'
#' path <- "C:/Users/sjnor/surfdrive/Shared/SAPPHIRE/Output/Presentations/Isolates_WS-Athens/data/dbase_isolates_V2.xlsx"
#' data <- get_sheetdata(path = path, sheets = "isolates")
#'
#' # 2. There are several sheets, each of which represents a glot. The name of the sheets are glottocodes.  The first column in each sheet contains glottosubcodes (abcd1234_a1)
#' get_sheetdata(path = "glottodata.xlsx", sheets = c("abcd1234", "abcd1235", "abcd1236") )
#' get_sheetdata(path = "glottodata.xlsx", sheets = c(1:16) )
#'
#' path <- "C:/Users/sjnor/surfdrive/Shared/Tanimuka_Yucuna/Databases/Voor_Sietze/NounPhrase_V6.xlsx"
#' get_sheetnames(path)
#' data <- get_sheetdata(path = path, sheets = c(1:16) )
#'
#' # 3. There are several sheets, each of which represents a subglot. The name of the sheets are glottosubcodes. The first column in each sheet contains glottosubsubcodes (abcd1234_a1_a1)
#' get_sheetdata(path = "glottodata.xlsx", sheets = c("abcd1234_form", "abcd1234_meaning", "abcd1235_form", "abcdf1235_meaning") )
#'
#' path <- "C:/Users/sjnor/surfdrive/Shared/Tanimuka_Yucuna/Databases/Voor_Sietze/TAME_V6.xlsx"
#' get_sheetnames(path)
# TOFIX: https://stackoverflow.com/questions/30177053/using-a-trycatch-block-to-read-a-csv-and-an-excel-file
get_sheetdata <- function(path, sheets = NULL, simplify = TRUE){
  if(is.null(sheets)){
    data <- base::lapply(X = readxl::excel_sheets(path),
                         FUN = readxl::read_excel, path = path)
  } else {
    data <- base::lapply(X = sheets, FUN = readxl::read_excel, path = path)
  }
  names(data) <- readxl::excel_sheets(path)[sheets]

  if(length(sheets) == 1 & simplify == TRUE){
    data <- readxl::read_xlsx(path = path, sheet = sheets)
  }

  return(data)
}

langlistmerger <- function(langlist = NULL){
  checkdata_lscolcount(langlist)
  do.call("rbind", langlist) # alternative approaches: data.table::rbindlist or plyr::rbind.fill
}



# Check metasheet: weight, type, colnames
