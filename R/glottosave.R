#' Save glottodata in relevant format
#'
#' If no file extention is provided, a sensible default is chosen. Dynamic maps
#' (tmap) are saved in .html format, static maps (tmap) are saved as .png.
#' Spatial data (sf) are saved as geopackage (.GPKG) by default, but .shp is
#' also possible.
#'
#' @param glottodata User-provided glottodata
#' @param filename Filename either with or without file extension
#' @family <glottodata>
#' @return
#' @export
#' @seealso get_glottodata
#' @examples
#'
#' glottodata <- get_glottodata(meta = FALSE)
#' Saves as .xlsx
#' glottosave(glottodata, filename = "glottodata")
#'
#' glottodata <- glottodata_makespatial(glottodata)
#' Saves as .GPKG
#' glottosave(glottodata, filename = "glottodata")
#'
#' glottomap <- glottomap(glottodata)
#' Saves as .png
#' glottosave(glottomap, filename = "glottomap")
#'
#' Saves as .html
#' glottomap <- glottomap(glottodata, type = "dynamic")

glottosave <- function(glottodata, filename = NULL){

  if((class(object) == "tmap")[1]){
    if(tools::file_ext(filename) == ""){
    ifelse(getOption("tmap.mode") == "plot", filename <- paste0(filename, ".png"), filename <- paste0(filename, ".html"))
    }
    tmap::tmap_save(object, filename = filename)
  } else if( is_sf(object) ){
    # if no file extension: gpkg
    if(tools::file_ext(filename) == ""){
      sf::st_write(obj = object, dsn = paste0(filename, ".gpkg"),
               append = FALSE)
    } else {
      sf::st_write(obj = object, dsn = filename,
               append = FALSE)
    }
  } else if(any(class(object) == "matrix" ) ){
    if(tools::file_ext(filename) == ""){
    utils::write.csv(object, file = paste0(filename, ".csv"))
    } else {
      utils::write.csv(object, file = filename)
    }
  } else if(class(object) == "data.frame"){
    if(tools::file_ext(filename) == ""){filename <- paste0(filename, ".xlsx")}
    writexl::write_xlsx(object, path = filename) # works better than openxlsx, which omits some columns..
  }

}

