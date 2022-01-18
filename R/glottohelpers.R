#' Create empty distance matrix
#'
#' @param names
#'
#' @return
#' @export
#' @noRd
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

geodata_rename_column <- function(geodata, oldname, newname){
  geodata %>%
  dplyr::mutate() %>% # https://github.com/r-spatial/sf/issues/1472 Empty mutate() also sets geom to last column
  dplyr::rename(newname = oldname)
}

glottovars <- function(){
  colnames(glottoget("glottobase"))
}

recode_df <- function(data, old, new){
  apply(data, MARGIN = 2, FUN = recode_vector, old = old, new = new)
}

recode_vector <- function(vector, old, new){
  vector[which(vector %in% old)] <- new
  return(vector)
}


