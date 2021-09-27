#' Download glottodata, or load user-provided glottodata
#'
#' Download glottodata, load glottodadata/glottosubdata from a file, or create artificial dummy data.
#'
#' @param filename Path to glottodata file with extension (.xlsx .xls .gpkg .shp). If no filename is specified, an artificial dummy dataset will be created.
#' @param meta By default, meta sheets are not loaded. Use meta=TRUE if you want to include them.
#' @param simplify By default, if only one sheet is loaded, the data will be returned as a data.frame (instead of placing the data inside a list of length 1)
#' @param create In case 'filename' is not specified, artificial dummy data will be created in glottodata format (specify create = "glottosubdata" to create data in glottosubdata format)
#' @family <glottodata>
#' @return
#' @export
#' @seealso glottosave
#' @examples
#' glottoget()
#' glottoget(filename = "glottodata.xlsx")
#' glottoget(filename = "glottodata.gpkg")
glottoget <- function(glottodata = NULL, ...){
  if(is.null(glottodata) ){
    glottodata <- get_glottodata(...)}
    else if(glottodata == "glottodata"){
    glottodata <- get_glottodata(...)
  } else if(glottodata == "glottosubdata"){
    glottodata <- get_glottodata(..., create = "glottosubdata")
  } else if(glottodata == "glottolog"){
    glottodata <- get_glottolog()
  } else if(glottodata == "glottobase"){
    glottodata <- get_glottobase()
  } else if (glottodata == "glottospace"){
    glottodata <- get_glottospace()
  } else {message("Unable to load requested glottodata")}

}

#' Load glottodata from an excel file
#'
#' Load glottodadata/glottosubdata from a file, or create artificial dummy data.
#'
#' @param filename Path to glottodata file with extension (.xlsx .xls .gpkg .shp). If no filename is specified, an artificial dummy dataset will be created.
#' @param meta By default, meta sheets are not loaded. Use meta=TRUE if you want to include them.
#' @param simplify By default, if only one sheet is loaded, the data will be returned as a data.frame (instead of placing the data inside a list of length 1)
#' @param create In case 'filename' is not specified, artificial dummy data will be created in glottodata format (specify create = "glottosubdata" to create data in glottosubdata format)
#' @family <glottodata>
#' @return
#' @export
#' @seealso glottosave
#' @examples
#' get_glottodata()
#' get_glottodata(filename = "glottodata.xlsx")
#' get_glottodata(filename = "glottodata.gpkg")
get_glottodata <- function(filename = NULL, meta = FALSE, simplify = TRUE, create = "glottodata"){


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
    if(create == "glottodata"){glottodata <- createdummydata()
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


#' Get glottobase reference data
#'
#' Downloads most recent glottolog data and transforms it. This 'glottobase' is used as reference dataset in several functions.
#'
#' @return
#' @export
#'
#' @examples
#' glottobase <- get_glottobase()
get_glottobase <- function(){
  glottolog <- get_glottolog(data = "glottolog")
  glottobase <- glottologbooster(glottologdata = glottolog)
  glottobase
}

#' Get glottospace reference data
#'
#' Get most recent glottolog data and turn it into the most elemental geoglot object (i.e. glottocodes + geometry column). This 'glottospace' is used as reference dataset in several functions.
#'
#' @return
#' @export
#' @seealso glottodata_addcoords
#'
#' @examples
#' glottospace <- get_glottospace()
get_glottospace <- function(){
  glottologdata <- get_glottolog(data = "glottolog")
  glottologdata <- glottologdata %>% dplyr::rename("glottocode" = "id")
  glottospace <- glot2geoglot(glottologdata)
  glottospace <- glottospace[,c("glottocode")]
}



