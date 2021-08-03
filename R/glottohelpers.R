# TODO: is_path # check whether something is a character string and path exists (platform independent)



unpack <- function(path = NULL){
  ftar <- list.files(path = path, pattern = ".tar", full.names = TRUE)
  if(!purrr::is_empty(ftar)){
    lapply(ftar, untar, exdir = path)
    f <- ftar
    ext <- ".tar"
  }

  fzip <- list.files(path = path, pattern = ".zip", full.names = TRUE)
  if(!purrr::is_empty(fzip)){
    lapply(fzip, unzip, exdir = path)
    f <- fzip
    ext <- ".zip"
  }

  message(paste(length(f), ext, " files unpacked to:", path))
}


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


