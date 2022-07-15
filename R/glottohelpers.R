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
  glottovars <- colnames(sf::st_drop_geometry(glottoget("glottobase")))
  vartext <- "If no glottodata object is provided, then you have the following options for the 'color' and 'label' arguments: "
  glottovars <- paste0(c(vartext, glottovars, ""), collapse = "', '")
  paste0("\\details{", glottovars, "}")
}

recode_df <- function(data, old, new){
  apply(data, MARGIN = 2, FUN = recode_vector, old = old, new = new)
}

recode_tona <- function(x, tona){
  x[which(x %in% tona)] <- NA
  x
}

recode_vector <- function(vector, old, new){
  vector[which(vector %in% old)] <- new
  return(vector)
}

#' Find R source script of a function
#'
#' @param funcname Either a function object (without brackets) or a character string
#'
#' @noRd
funcsource <- function(funcname){
  if(is.character(funcname)){funcname <- get(funcname)}
  # https://stackoverflow.com/questions/32747242/find-the-source-file-containing-r-function-definition/32749240#32749240
  srcfile <- attr(attr(funcname,"srcref"),"srcfile")
  srcfile$filename
}

findstring <- function(string, dir = NULL){
  if(is.null(dir)){dir <- "C:/Users/sjnor/surfdrive/PROJECTS_SN/Rpackages/glottospace/R"}
  rfiles <- list.files(path = dir, pattern = ".R")

  for (x in seq_along(rfiles) ) {
    if (length(grep(string, readLines(paste0(dir, "/", rfiles[x])))) > 0) { print(rfiles[x])}
  }
}

contrans_id2gc <- function(id){
  ifelse(is.null(id), id <- "glottocode", id)
}

is_false <- function(x) {
  # NAs are kept
  x == FALSE
}

is_true <- function(x) {
  # NAs are kept
  x == TRUE
}

is_dist <- function(object){
  inherits(object, what = "dist" )
}

is_list <- function(object){
  inherits(object, what = "list" )
}

contrans_tb2df <- function(glottodata){
  if(tibble::is_tibble(glottodata)){
    return(as.data.frame(glottodata))
  } else {
    return(glottodata)
  }

}

#' Sum of vector elements
#'
#' Calculate sum of all vector elements that are not NA. If all elements are NA, NA will be returned (in contrast to sum).
#'
#' @param v A vector
#' @noRd
sumna <- function(v){
  if(all(is.na(v)) ){
    return(NA)
  } else {
    return(sum(v, na.rm = TRUE))
  }
  # ifelse(all(is.na(v)), NA, sum(v, na.rm = TRUE))
}

#' Select non-NA elements from vector
#'
#' Select non-NA elements from vector. If all elements are NA, NA will be returned.
#' @noRd
#' @param v
#'
nonna <- function(v, max1 = FALSE){
  sel <- v[!is.na(v)]
  if(purrr::is_empty(sel)){sel <- NA}
  if(length(sel) > 1 & max1 == TRUE){stop("More than one element is non-NA, specify max1 = FALSE if you want to allow for multiple matches.")}
  return(sel)
}

#' Unpack .tar or .zip files
#'
#' @param optional path to .tar or .zip file
#' @noRd
unpack <- function(path = NULL){
  ftar <- list.files(path = path, pattern = ".tar", full.names = TRUE)
  if(!purrr::is_empty(ftar)){
    lapply(ftar, utils::untar, exdir = path)
    f <- ftar
    ext <- ".tar"
  }

  fzip <- list.files(path = path, pattern = ".zip", full.names = TRUE)
  if(!purrr::is_empty(fzip)){
    lapply(fzip, utils::unzip, exdir = path)
    f <- fzip
    ext <- ".zip"
  }

  message(paste(length(f), ext, " files unpacked to:", path))
}


#' Generate warning message instead of print
#' @noRd
printmessage <- function(x)
{
  message(paste(utils::capture.output(print(x)), collapse = "\n"))
}

#' Negated in
#'
#' Added here to remove dependency on Hmisc
#'
#' @export
#' @keywords internal
`%nin%` <- function(a,b){
  ! a %in% b
  # `%nin%` <- Negate(`%in%`)
  }

release_questions <- function() {
  c(
    "Have you updated all internal datasets (run functions in data-raw)?",
    "Did you go outside for a walk?"
  )
}


