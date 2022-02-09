#' Save glottodata, maps and plots
#'
#' If no filename is provided, the name of the glottodata object will be used.
#'
#' If no file extension is provided, a sensible default file extension is chosen. Dynamic maps
#' (tmap) are saved in .html format, static maps (tmap) are saved as .png.
#' Spatial data (sf) are saved as geopackage (.GPKG) by default, but .shp is
#' also possible.
#'
#' @param glottodata User-provided glottodata
#' @param filename Filename either with or without file extension
#' @family <glottodata>
#' @return No object is returned, it will be save locally at the specified location
#' @export
#' @seealso glottoget_glottodata
#' @examples
#' \donttest{
#' glottodata <- glottoget("demodata", meta = FALSE)
#' # Saves as .xlsx
#' glottosave(glottodata, filename = "glottodata")
#' glottosave(glottodata, filename = file.path(tempdir(), "glottodata") )
#'
#' glottospacedata <- glottospace(glottodata)
#' # Saves as .GPKG
#' glottosave(glottodata, filename = "glottodata")
#'
#' glottomap <- glottomap(glottodata)
#' # Saves as .png
#' glottosave(glottomap, filename = "glottomap")
#'
#' # Saves as .html
#' glottomap <- glottomap(glottodata, type = "dynamic")
#' }
glottosave <- function(glottodata, filename = NULL){

  if(is.null(filename)){filename <- deparse(substitute(glottodata))}

  if(glottocheck_isglottodata(glottodata)){
    if(tools::file_ext(filename) != ".xlsx"){
      filename <- tools::file_path_sans_ext(filename)
      filename <- paste0(filename, ".xlsx")
    }
    # if(file.exists(filename) & overwrite == FALSE){stop("File already exists, use overwrite = TRUE")}
    writexl::write_xlsx(glottodata, path = filename) # works better than openxlsx, which omitted some columns..
    message(paste("Glottodata (glottodata) saved as", filename))
  } else if(glottocheck_isglottosubdata(glottodata)){
      if(tools::file_ext(filename) != ".xlsx"){
        filename <- tools::file_path_sans_ext(filename)
        filename <- paste0(filename, ".xlsx")
      }
      writexl::write_xlsx(glottodata, path = filename) # works better than openxlsx, which omitted some columns..
      message(paste("Glottodata (glottodata) saved as", filename))
  } else if((class(glottodata) == "tmap")[1]){
    if( tools::file_ext(filename) == "" ){
    ifelse(getOption("tmap.mode") == "plot", filename <- paste0(filename, ".png"), filename <- paste0(filename, ".html"))
    }
    tmap::tmap_save(glottodata, filename = filename)
    message(paste0("Map (tmap object) saved as ", filename))
  } else if( is_sf(glottodata) ){
    if( tools::file_ext(filename) == "" ){
      sf::st_write(obj = glottodata, dsn = paste0(filename, ".gpkg"),
               append = FALSE)
    } else {
      sf::st_write(obj = glottodata, dsn = filename,
               append = FALSE)
    }
    message(paste0("Spatial object (sf) saved as ", filename))
  } else if(any(class(glottodata) == "matrix" ) ){
    if( tools::file_ext(filename) == "" ){
    utils::write.csv(glottodata, file = paste0(filename, ".csv"))
    } else {
      utils::write.csv(glottodata, file = filename)
    }
    message(paste0("Matrix saved as ", filename))
  } else if(class(glottodata) == "data.frame"){
    if( tools::file_ext(filename) == "" ){filename <- paste0(filename, ".xlsx")}
    writexl::write_xlsx(glottodata, path = filename) # works better than openxlsx, which omits some columns..
    message(paste0("Data.frame saved as ", filename))
  } else(
    message("Could not detect object type. Please convert glottodata to a supported object type.")
  )

}

