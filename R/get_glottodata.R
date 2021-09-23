#' Load glottodata from an excel file
#'
#' Load glottodadata/glottosubdata from a file, or create artificial dummy data.
#'
#' @param filename Path to glottodata file with extension (.xlsx .xls .gpkg .shp). If no filename is specified, an artificial dummy dataset will be created.
#' @param meta By default, all sheets are loaded, including all meta sheets. Use meta=FALSE if you want to ignore metasheets
#' @param simplify By default, if only one sheet is loaded, the data will be returned as a data.frame (instead of placing the data inside a list of length 1)
#' @param dummy If no filename is specified, dummy data will be created in glottodata format (specify dummy = "glottosubdata" to create data in glottosubdata format)
#' @family <glottodata>
#' @return
#' @export
#' @seealso glottosave
#' @examples
#' get_glottodata()
#' get_glottodata(filename = "glottodata.xlsx")
#' get_glottodata(filename = "glottodata.gpkg")
get_glottodata <- function(filename = NULL, meta = TRUE, simplify = TRUE, dummy = "glottodata"){


  metasheets <- c("structure",  "metadata",   "references", "readme",     "lookup" )

  if(!is.null(filename)){
    if(tools::file_ext(filename) == ".xlsx" | tools::file_ext(filename) == ".xls"){
    sheetnames <- readxl::excel_sheets(filename)
  if(meta == TRUE){
    sheetnames <- sheetnames
  } else {
    sheetnames <- sheetnames[sheetnames %nin% metasheets]
  }
  glottodata <- base::lapply(X = sheetnames,
                         FUN = readxl::read_excel, path = filename)
  names(glottodata) <- sheetnames
    } else if(tools::file_ext(filename) == ".gpkg" | tools::file_ext(filename) == ".shp"){
      glottodata <- sf::st_read(dsn = filename)
    }

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

  if(simplify == TRUE & length(glottodata) == 1 & any(class(glottodata) == "list") ){
    glottodata <- glottodata[[1]]
  }

  return(glottodata)
}





