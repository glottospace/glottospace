
glotto_addfeatures <- function(){

}

# # TODO: Match sources to languages by
# head(glottolog_cldf$sources[,"LGCODE"])

glottolog_addfamilyname <- function(glottodata = NULL){
  #' @importFrom magrittr %>%
  if(is.null(glottodata)){
    glottodata <- getglottodata()
  message("No input data provided, glottolog data downloaded")
  }

if(!base::exists("glottodata") | base::is.null(glottodata) ){
    glottodata <- getglottodata()
    message("No input data provided, glottolog data downloaded")
  }
}

  # if(nrow(filter(glottodata, level == "family")) == 0){
  #   message("No family names added because input data does not contain any. If you want to add family names, download full glottolog dataset.")
  # } else{
  #
  #   families <- glottodata %>%
  #     dplyr::filter(level == "family") %>%
  #     dplyr::filter(family_id == "" & parent_id == "") %>%
  #     dplyr::transmute(family_id = id, family_name = name)
  #
  #   data <- dplyr::left_join(x = glottodata, y = families, by = "family_id")
  # }



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
# if(isolates == TRUE){
#   n <- nrow(data[(data$family_id == "") & (is.na(data$family_name)), ])
#   # set family name to isolate
#   data <- data %>% mutate(family_name = replace(family_name,
#                                                 is.na(family_name), "isolate"))
#   # Base R, generate sequential unique family_id
#   data[data$family_id == "", "family_id"] <- sprintf("isol%04d", seq(1,n))
# }
#
# if(family_size == TRUE){
#   data <- data %>%
#     group_by(family_id) %>%
#     mutate(family_size = n())
#
#   # add family size rank
#   data$family_size_rank <- as.factor(data$family_size)
#   levels(data$family_size_rank) <- seq(1:length(levels(data$family_size_rank)))
#   # data$family_size_rank  <- ordered(data$family_size_rank)
#   data$family_size_rank  <- as.numeric(data$family_size_rank) # easier plotting than ordered levels
# }
