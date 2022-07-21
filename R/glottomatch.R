#' glottomatch
#'
#' Match a vector of language names to glottocodes and names
#'
#' @param namevec Vector of language names
#' @param glottodata Optional, where to search for matches. If kept empty, the entire glottolog database will be searched, you could also search within a specific area
#' @param tolerance Optional, search tolerance.
#'
#' @return a data.frame with exact or closest matches, and their glottocodes.
#' @export
#'
#' @examples
#' glottodata <- glottofilter(continent = "South America")
#' # Finds a single match
#' glottomatch(name = "yucuni", glottodata = glottodata)
#' # Finds multiple matches
#' glottomatch(name = "quechui", glottodata = glottodata)
glottomatch <- function(namevec, glottodata = NULL, tolerance = NULL){
  if(is.null(glottodata)){glottodata <- glottoget()}
  glottodata <- sf::st_drop_geometry(glottodata[,c("glottocode", "name")])

  if(is.null(tolerance)){tolerance <- 0.1}

  namevec <- unlist(namevec)

  resultsdf <- data.frame("name" = character(), "matchname"  = character(), "glottocode"  = character())

  for(i in 1:length(namevec)){
    add <- glottomatch_one(name = namevec[i], glottodata = glottodata)
    resultsdf <- rbind(resultsdf, add)
  }

  resultsdf

}

glottomatch_one <- function(name, glottodata, tolerance = NULL){

  if(is.null(tolerance)){tolerance <- 0.1}

  if(is.na(name)){
    matchname <- NA
    matchgc <- NA
  } else{
    found <- glottosearch(search = name, glottodata = glottodata, partialmatch = FALSE, columns = "name")
    if(nrow(found)!=0){
      matchname <- found[,"name"]
      matchgc <- found[,"glottocode"]
    } else{
      found <- glottosearch(search = name, glottodata = glottodata, partialmatch = TRUE, columns = "name", tolerance = 0.1)
      if(nrow(found)!=0){
        matchname <- found[,"name"]
        matchgc <- found[,"glottocode"]
      } else{
        found <- glottosearch(search = name, glottodata = glottodata, partialmatch = TRUE, columns = "name", tolerance = 0.15)
        if(nrow(found)!=0){
          matchname <- found[,"name"]
          matchgc <- found[,"glottocode"]
        } else{
          found <- glottosearch(search = name, glottodata = glottodata, partialmatch = TRUE, columns = "name", tolerance = 0.2)
          if(nrow(found)!=0){
            matchname <- found[,"name"]
            matchgc <- found[,"glottocode"]
          } else{
            found <- glottosearch(search = name, glottodata = glottodata, partialmatch = TRUE, columns = "name", tolerance = 0.25)
            if(nrow(found)!=0){
              matchname <- found[,"name"]
              matchgc <- found[,"glottocode"]
            } else{
              found <- glottosearch(search = name, glottodata = glottodata, partialmatch = TRUE, columns = "name", tolerance = 0.3)
              if(nrow(found)!=0){
                matchname <- found[,"name"]
                matchgc <- found[,"glottocode"]
              } else{
                matchname <- NA
                matchgc <- NA
              }
            }
          }
        }
      }
    }
  }

  resultsdf <- data.frame("name" = rep(NA,
                                       ifelse(nrow(found)==0, 1, nrow(found))
  ),
  "matchname" = NA, "glottocode" = NA)

  resultsdf[,"name"] <- name
  resultsdf[, "matchname"] <- matchname
  resultsdf[, "glottocode"] <- matchgc
  resultsdf
}
