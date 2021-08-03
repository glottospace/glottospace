# save any glottospace/geoglot object in relevant format
# dynamic/static maps
# empty glottospace object
# spatial object as GPKG

glottosave <- function(object = NULL, filename = NULL){

  if((class(object) == "tmap")[1]){
    filename <- sub(pattern = "(.*)\\..*$", replacement = "\\1", filename)
    tmap_save(object, filename = filename)
  }
  if((class(object) == "sf")[1]){
    if (!require(tools)) {install.packages('tools')}
    library(tools)
    # if no file extension: gpkg
    if(file_ext(filename) == ""){
      st_write(obj = object, dsn = paste0(filename, ".gpkg"),
               append = FALSE)
    } else {
      st_write(obj = object, dsn = filename,
               append = FALSE)
    }
  }
}
