
#' Find R source script of a function
#'
#' @param funcname Either a function object (without brackets) or a character string
#'
#' @return
#' @noRd
funcsource <- function(funcname){
  if(is.character(funcname)){funcname <- get(funcname)}
  # https://stackoverflow.com/questions/32747242/find-the-source-file-containing-r-function-definition/32749240#32749240
  srcfile <- attr(attr(funcname,"srcref"),"srcfile")
  srcfile$filename
}

contrans_char2obj <- function(){}

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

#' Highlight certain data points in visualizations
#'
#' This function creates two separate color scales: one for points to highlight,
#' and a second for the remaining background points. It also creates a legend.
#' This is useful for preparing the data for visualizations such as maps or
#' other plots.
#'
#' @param glottodata User-provided glottodata
#' @param spotcol Name of the column that contains the data to put in the spotlights, as well as remaining background data.
#' @param spotlight Selection of data to put in the spotlights.
#' @param spotcontrast Optional column to contrast between data points in the spotlight.
#' @param bgcontrast Optional column to contrast between background data points
#'
#' @return
#' @export
#'
#' @examples
#' glottodata <- glottofilter(country = c("Netherlands", "Germany", "Belgium") )
#' glottodata <- glottospotlight(glottodata = glottodata, spotcol = "country", spotlight = "Netherlands", spotcontrast = "name")
#' glottomap(glottodata, color = "color")
glottospotlight <- function(glottodata, spotcol, spotlight, spotcontrast = NULL, bgcontrast = NULL){
  data <- sf::st_drop_geometry(glottodata)
  data$spotlight <- data[,spotcol] %in% spotlight

  data$legend <- NA

  if(is.null(bgcontrast)){
    data$legend <- ifelse(data$spotlight == FALSE, data[,spotcol], data$legend)
  } else{
    data$legend <- ifelse(data$spotlight == FALSE, data[,bgcontrast], data$legend)
  }

  if(is.null(spotcontrast)){
    data$legend <- ifelse(data$spotlight == TRUE, data[,spotcol], data$legend)
  } else{
    data$legend <- ifelse(data$spotlight == TRUE, data[,spotcontrast], data$legend)
  }

  spotlightnames <- as.factor(data$legend[data$spotlight == TRUE])
  ncolrspot <- length(unique(spotlightnames))
  colpalspot <- rainbow(ncolrspot)

  bgnames <- as.factor(data$legend[data$spotlight == FALSE])
  ncolrbg <- length(unique(bgnames))
  colpalbg <- grey.colors(ncolrbg)

  data$color[data$spotlight == TRUE] <- colpalspot[spotlightnames]
  data$color[data$spotlight == FALSE] <- colpalbg[bgnames]

  suppressMessages(dplyr::left_join(glottodata, data[, c("glottocode", "spotlight", "legend", "color")]))
}

# xfun::gsub_dir(dir = "C:/Users/sjnor/surfdrive/PROJECTS_SN/SAPPHIRE/R/glottospace/R", pattern = "dummy", replacement = "demo")

is_dist <- function(object){
  any(class(object) == "dist")
}

is_list <- function(object){
  class(object) == "list"
}
