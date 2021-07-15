
path <- "D:/Global/Biodiversity/wwf_terr_ecos/wwf_terr_ecos.shp"
path <- "D:/Global/Topography/Geomorpho90m/250m/Global/dtm_roughness_merit.dem_m_250m_s0..0cm_2018_v1.0.tif"
path <- "D:/Global/Soils/HWSD/1km/Global/sq1.asc"


#' Open geodata (raster or vector format)
#'
#' @param path Path to raster or vector file
#' @param name Optional
#'
#' @return
#' @export
#'
#' @examples
spatialfilter <- function(path, name = NULL){
  rasvec <- get_rasvec(path = path)

  if(is_raster(rasvec)){
    if(!is.null(name)){names(ras) <- name}
  }

  if(is_vector(rasvec)){

  }
}

#' Get spatial data
#'
#' Open spatial data from a local path (supports both raster and vector formats).
#'
#' @param path Path to raster or vector data
#'
#' @return
#'
#' @examples
get_spatialdata <- function(path){

  if(!file.exists(path)){stop("Path not found")}


  # vec <- try(sf::st_read(path), silent = TRUE)
  # if(all(class(vec) != "try-error")){message("Vector data loaded")}
  vec <- tryCatch(sf::st_read(path), error=function(e){})
  if(!is.null(vec)){ras <- NULL
  } else {
    ras <- tryCatch(raster::raster(path), error=function(e){})
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

rasterstack <- function(){}
rastermosaic <- function(){}
vectormerge <- function(){}
rasterfilter <- function(){}

#
#   if(name == "worldclim"){
#     if(res == "1km" & region == "South America"){file <- paste("wc2.0_bio_30s_", sprintf("%02d", seq(1,19)), '.tif', sep='')}
#     paths <- gs_geopath(pathgroup = "Climate/WORLDCLIM2", res = "1km", region = "South America", file = file)
#     stack <- raster::stack(paths)
#     names(stack) <- paste0("bio", sprintf("%02d", seq(1,19)))
#     message(paste("Loading raster:", name, "at", res, "resolution for:", region))
#     if (region == "Global"){
#       stack <- raster::getData(name = "worldclim", var = "bio", res = 10)
#     }
#     if(selection == "default"){
#       ras <- stack
#     } else {
#       ras <- stack[[selection]]
#     }
#     if(!is.null(ras)){
#       out <- ras
#       cat(paste("WorldClim 2.0 data loaded (", file, ") \n"))
#     }
#   }
#
#   if(name == "ecoregion"){
#     file <- "wwf_terr_ecos.shp"
#     path <- gs_geopath(pathgroup = "Biodiversity/wwf_terr_ecos", file = file)
#     vec <- st_read(path)
#     cat(paste("Loading shapefile:", name))
#     if(!is.null(vec)){
#       if(selection == "default"){
#         vec <- vec[, "ECO_NAME"]
#         colnames(vec)[1] <- name
#       } else if(selection != "default" & is.character(selection)){
#         vec <- vec[, selection]
#       } else if(selection == "all" | is.null(selection)){
#         vec <- vec
#       }
#       out <- vec
#       cat(paste0("Terrestrial Ecoregions of the World loaded (", file, ")"))
#     }
#   }
#
#   if(name == "rivervect"){
#     if(region == "South America"){file <- "South America_constant_Dd.gpkg"}
#     path <- gs_geopath(pathgroup = "Hydrography/drainage density", region = region, file = file)
#     vec <- st_read(path)
#     cat(paste("Loading shapefile:", name))
#     if(!is.null(vec)){
#       if(selection == "default"){
#         vec <- vec
#       } else if(selection != "default" & is.character(selection)){
#         vec <- vec[, selection]
#       } else if(selection == "all" | is.null(selection)){
#         vec <- vec
#       }
#       out <- vec
#       cat(paste0("River network constant Dd loaded (", file, ") for region: ", region))
#     }
#   }
#
