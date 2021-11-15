#' Color palettes for maps and figures
#'
#' @param palette Name of color palette, use glottocolpal("all") to see the options
#' @param ncol Number of colors to be generated (optional)
#' @param rev Should the order of colors be reversed?
#' @param alpha Values between 0 (transparent) and 1 (opaque)
#' @param show Do you want to plot the color palette?
#'
#' @return
#' @export
#'
#' @examples
#' glottocolpal(palette = "turbo", show = TRUE)
glottocolpal <- function(palette, ncol = NULL, rev = FALSE, alpha = NULL, show = FALSE){
  viridispals <- c("viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo")
  colorbrewerpals <- rownames(RColorBrewer::brewer.pal.info)
  grdevpals <- grDevices::hcl.pals()
  grdevpals <- grdevpals[order(grdevpals)]

  if(is.null(palette)){
    return(NULL)
  }

  if(palette == "all"){
    return(glottocolpal_options())
  }

  if(palette %in% viridispals){
    if(is.null(ncol)){ncol <- 20}
    colpal <- viridisLite::viridis(n = ncol, option = palette, alpha = alpha, direction = {ifelse(rev == FALSE, 1, -1)})
  } else if(palette %in% grdevpals){ # contains palettes from RColorBrewer and viridis. grDevices is more flexible than RColorBrewer and therefore is prioritized
    if(is.null(ncol)){ncol <- 20}
    colpal <- grDevices::hcl.colors(n = ncol, palette = palette, rev = rev, alpha = alpha)
  } else if(palette %in% colorbrewerpals){
    if(is.null(ncol)){ncol <- RColorBrewer::brewer.pal.info[colorbrewerpals == palette, "maxcolors"]}
    colpal <- RColorBrewer::brewer.pal(ncol, palette)
    if(rev == TRUE){colpal <- rev(colpal)}
    }

  if(show == TRUE){
    scales::show_col(colours = colpal)
  }
  return(colpal)
}

#' Show which color palettes are available
#' @keywords internal
#' @return
#' @export
#'
glottocolpal_options <- function(){
  viridispals <- c("viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo")
  colorbrewerpals <- rownames(RColorBrewer::brewer.pal.info)
  grdevpals <- grDevices::hcl.pals()
  grdevpals <- grdevpals[order(grdevpals)]

  cat("Viridis palettes: \n")
  print(viridispals)
  cat("\n\n")

  cat("grDevices palettes: \n")
  print(grdevpals)
  cat("\n\n")

  cat("ColorBrewer palettes: \n")
  print(colorbrewerpals)
  cat("\n\n")
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
glottospotlight <- function(glottodata, spotcol, spotlight, spotcontrast = NULL, bgcontrast = NULL, palette = NULL){
  if(is.null(palette)){palette = "rainbow"}
  if(is_sf(glottodata)){
    data <- sf::st_drop_geometry(glottodata)
    was_sf <- TRUE
  } else {
    data <- glottodata
    was_sf <- FALSE
  }

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
  colpalspot <- glottocolpal(palette = palette, ncolrspot)

  bgnames <- as.factor(data$legend[data$spotlight == FALSE])
  ncolrbg <- length(unique(bgnames))
  colpalbg <- grey.colors(ncolrbg)

  data$color[data$spotlight == TRUE] <- colpalspot[spotlightnames]
  data$color[data$spotlight == FALSE] <- colpalbg[bgnames]

  if(was_sf == TRUE){
    glottodata <- suppressMessages(dplyr::left_join(glottodata, data[, c("glottocode", "spotlight", "legend", "color")]))
    return(glottodata)
  } else {
    return(data)
  }
}

glottospotlight_legend <- function(glottodata){
  if(all(c("spotlight", "legend", "color") %in% colnames(glottodata))){
    legend <- list(
      spotlight = TRUE,
      labels = unique(glottodata[sf::st_drop_geometry(glottodata[,"spotlight"]) == TRUE,]$legend),
      col = unique(glottodata[sf::st_drop_geometry(glottodata[,"spotlight"]) == TRUE,]$color))
  } else {
    legend <- list(spotlight = FALSE)
  }
  legend
}
