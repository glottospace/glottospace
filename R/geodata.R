
path <- "D:/Global/Biodiversity/wwf_terr_ecos/wwf_terr_ecos.shp"
path <- "D:/Global/Topography/Geomorpho90m/250m/Global/dtm_roughness_merit.dem_m_250m_s0..0cm_2018_v1.0.tif"
path <- "D:/Global/Soils/HWSD/1km/Global/sq1.asc"
path <- "D:/Global/Societal/travel time/travel_time_to_cities_1.tif"

#' Get spatial data
#'
#' Load spatial data either from local path (supports both raster and vector formats), or download from remote server.
#' @param path Path to geodata
#' @param download Name of geodata to download (wrapper around raster::getData). Currently, the following are supported: "climate"
#'
#' @return
#' @export
#'
#' @examples
get_geodata <- function(path = NULL, download = NULL){
  if(!is.null(path) & is.null(download)){
    geodata <- get_geodata_path(path = path)
  }
  if(is.null(path) & !is.null(download)){
    geodata <- get_geodata_download(download = download)
  }
  return(geodata)
}


#' Get spatial data from local path
#'
#' Open spatial data from a local path (supports both raster and vector formats).
#'
#' @param path Path to raster (RasterLayer or RasterStack) or vector data
#'
#' @return
#' @export
#'
#' @examples
get_geodata_path <- function(path){

  if(!file.exists(path)){stop("Path not found")}


  # vec <- try(sf::st_read(path), silent = TRUE)
  # if(all(class(vec) != "try-error")){message("Vector data loaded")}
  vec <- tryCatch(sf::st_read(path), error=function(e){})
  if(!is.null(vec)){ras <- NULL
  } else {
    ras <- tryCatch(raster::raster(path), error=function(e){})
    if(!is.null(ras) & ras@file@nbands != 1) {
      ras <- tryCatch(raster::stack(path), error=function(e){})
    }
  }

  if(is.null(vec) & is.null(ras)){
    stop(paste0("Cannot open ", path))
    }

  if(is.null(vec) & !is.null(ras)){
    rasvec <- ras
    message("Raster data loaded")
    }

if(!is.null(vec) & is.null(ras)){
    rasvec <- vec
    message("Vector data loaded")
    }

  return(rasvec)

}

get_geodata_download <- function(download){
  if(download == "climate"){
  raster::getData(name = "worldclim", var = "bio", res = 10)
  }
}



