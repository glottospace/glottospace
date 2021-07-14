
#' Create empty distance matrix
#'
#' @param names
#'
#' @return
#' @export
#'
#' @examples
#' gs_emptydistmat(names = glottocodes)
gs_emptydistmat <- function(names){
  outputmat <- matrix(data = NA,
                      nrow = length(names),
                      ncol = length(names),
                      dimnames = list(names, names))
  return(outputmat)
}


as.ordfact <- function(x = NULL, levels = NULL){ # alternatively, use: https://forcats.tidyverse.org/
  dtf <- as.factor(as.matrix(x))
  lvl <- unlist(strsplit(x = levels, split = '[,;]+'))
  dtot <- factor(x = dtf, levels = lvl, ordered = TRUE)
  df <- as.data.frame(dtot)
  return(df)
}

## From https://geocompr.robinlovelace.net/reproj-geo-data.html
lonlat2UTM = function(lonlat) {
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}
