
#
#
#
# if(countries == T){
#   if (!require(rnaturalearth)) {install.packages('rnaturalearth')}
#   library(rnaturalearth)
#   if (!require(sf)) {install.packages('sf')}
#   library(sf)
#   # TO ADD: check CRS identical
#
#   # Adding names of countries and continents
#   world <- ne_countries(returnclass = "sf", scale = "medium")
#   world <- world[, c("name", "continent", "subregion")]
#   names(world)[1] <- "country"
#   data <- st_join(x = data, y = world, left = TRUE)
# }
#
#
