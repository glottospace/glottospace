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
#' @param simplify If only one sheet is selected, should the data be returned without placing it inside a list (which would be a list of length one)?
#'
#' @return List with tibble per sheet
#' @export
#'
#' @examples
#' # 1. There is one sheet in which each row represents a glot (anything with a glottocode). The first column contains the glottocodes.
#' get_sheetdata(path = "glottodata.xlsx")
#' get_sheetdata(path = "glottodata.xlsx", sheets = 1)
#' get_sheetdata(path = "glottodata.xlsx", sheets = "glottodata")
#' # 2. There are several sheets, each of which represents a glot. The name of the sheets are glottocodes.  The first column in each sheet contains glottosubcodes (abcd1234_a1)
#' get_sheetdata(path = "glottodata.xlsx", sheets = c("abcd1234", "abcd1235", "abcd1236") )
#' get_sheetdata(path = "glottodata.xlsx", sheets = c(1:16) )
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




gs_langsheetmerger <- function(langlist = NULL, col = NULL, newvarname = NULL){
  # count columns check (see below)
  #' @param langlist List of languages.
  #' @return df Data.frame with all languages
  if(is.null(col)){
    # Default is to merge by row
    m <- do.call("rbind", langlist) # alternative approaches: data.table::rbindlist or plyr::rbind.fill
    df <- as.data.frame(m)
  }
  if(!is.null(col)){
    langlistdata <- lapply(langlist, `[`, col)
    ls <- unlist(langlistdata, recursive = F) # alternative is to transpose t()
    m <- do.call("rbind", ls)
    df <- as.data.frame(m)
    rownames(df) <- gsub(pattern = "\\.", replacement = "_", x = rownames(df))
    # rownames(df) <- names(langlist)
    if(!is.null(newvarname)){
      varnames <- lapply(langlist, `[`, newvarname)
      varnmdf <- as.data.frame(lapply(varnames, cbind))
      nvar <- apply(X = varnmdf, MARGIN = 1, FUN = unique)
      nvar <- unlist(lapply(nvar, length))
      if(!all(nvar == 1)){message("Not all varnames are identical across languages")}
      colnames(df) <- unlist(varnames[[1]])
    }
  }

  return(df)
}

# CHECK ACROSS ALL ELEMENTS OF A LIST
check_colcountsame <- function(langlist){
  # Check whether number of columns are identical across languages
  colcount <- lapply(X = lsls, FUN = function(x){length(colnames(x))})
  colcount <- unlist(colcount, recursive = F)
  if(length(unique(colcount)) > 1){
    message('Not all languages have same number of features \n')
    message(paste(glottocodes, ": ", colcount, "\n"))
  }

}

# Check metasheet: weight, type, colnames
