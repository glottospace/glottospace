#' Load glottodata from an excel file
#'
#' Load glottodadata/glottosubdata from an excel file, or create artificial dummy data.
#'
#' @param filename Path to excel file (.xls or .xlsx). If no filename is specified, an artificial dummy dataset will be created.
#' @param meta By default, all sheets are loaded, including all meta sheets. Use meta=FALSE if you want to ignore metasheets
#' @param simplify By default, if only one sheet is loaded, the data will be returned as a data.frame (instead of placing the data inside a list of length 1)
#' @param dummy If no filename is specified, dummy data will be created in glottodata format (specify dummy = "glottosubdata" to create data in glottosubdata format)
#'
#' @return
#' @export
#'
#' @examples
#' get_glottodata()
#' get_glottodata(filename = "glottodata.xlsx")
get_glottodata <- function(filename = NULL, meta = TRUE, simplify = TRUE, dummy = "glottodata"){


  metasheets <- c("structure",  "metadata",   "references", "readme",     "lookup" )

  if(!is.null(filename)){
    sheetnames <- readxl::excel_sheets(filename)
  if(meta == TRUE){
    sheetnames <- sheetnames
  } else {
    sheetnames <- sheetnames[sheetnames %nin% metasheets]
  }
  glottodata <- base::lapply(X = sheetnames,
                         FUN = readxl::read_excel, path = filename)
  names(glottodata) <- sheetnames


  } else {
    if(dummy == "glottodata"){glottodata <- createdummydata()
    } else {
      glottodata <- createdummysubdata()
    }
    sheetnames <- names(glottodata)
    if(meta == TRUE){
      sheetnames <- sheetnames
    } else {
      sheetnames <- sheetnames[sheetnames %nin% metasheets]
    }

    glottodata <- glottodata[sheetnames]

  }

  if(simplify == TRUE & length(glottodata) == 1 & class(glottodata) == "list"){
    glottodata <- glottodata[[1]]
  }

  return(glottodata)
}





