#' Color palettes for maps and figures
#'
#' @param palette Name of color palette, use glottocolpal("all") to see the options
#' @param ncolr Number of colors to be generated (optional)
#' @param rev Should the order of colors be reversed?
#' @param alpha Values between 0 (transparent) and 1 (opaque)
#' @param show Do you want to plot the color palette?
#'
#' @return A vector of colors, optionally plotted.
#' @export
#' @keywords internal
#' @examples
#' glottocolpal(palette = "turbo", show = TRUE)
glottocolpal <- function(palette, ncolr = NULL, rev = FALSE, alpha = NULL, show = FALSE){
  viridispals <- c("viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo")
  # dput(as.character(rownames(RColorBrewer::brewer.pal.info))) # intermediate step to reduce dependencies. Output pasted below
  colorbrewerpals <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
                       "Spectral", "Accent", "Dark2", "Paired", "Pastel1", "Pastel2",
                       "Set1", "Set2", "Set3", "Blues", "BuGn", "BuPu", "GnBu", "Greens",
                       "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples",
                       "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")
  # dput(as.character(grDevices::hcl.pals())) # intermediate step to reduce dependencies. Output pasted below
  grdevpals <- c("Pastel 1", "Dark 2", "Dark 3", "Set 2", "Set 3", "Warm", "Cold",
                 "Harmonic", "Dynamic", "Grays", "Light Grays", "Blues 2", "Blues 3",
                 "Purples 2", "Purples 3", "Reds 2", "Reds 3", "Greens 2", "Greens 3",
                 "Oslo", "Purple-Blue", "Red-Purple", "Red-Blue", "Purple-Orange",
                 "Purple-Yellow", "Blue-Yellow", "Green-Yellow", "Red-Yellow",
                 "Heat", "Heat 2", "Terrain", "Terrain 2", "Viridis", "Plasma",
                 "Inferno", "Rocket", "Mako", "Dark Mint", "Mint", "BluGrn", "Teal",
                 "TealGrn", "Emrld", "BluYl", "ag_GrnYl", "Peach", "PinkYl", "Burg",
                 "BurgYl", "RedOr", "OrYel", "Purp", "PurpOr", "Sunset", "Magenta",
                 "SunsetDark", "ag_Sunset", "BrwnYl", "YlOrRd", "YlOrBr", "OrRd",
                 "Oranges", "YlGn", "YlGnBu", "Reds", "RdPu", "PuRd", "Purples",
                 "PuBuGn", "PuBu", "Greens", "BuGn", "GnBu", "BuPu", "Blues",
                 "Lajolla", "Turku", "Hawaii", "Batlow", "Blue-Red", "Blue-Red 2",
                 "Blue-Red 3", "Red-Green", "Purple-Green", "Purple-Brown", "Green-Brown",
                 "Blue-Yellow 2", "Blue-Yellow 3", "Green-Orange", "Cyan-Magenta",
                 "Tropic", "Broc", "Cork", "Vik", "Berlin", "Lisbon", "Tofino",
                 "ArmyRose", "Earth", "Fall", "Geyser", "TealRose", "Temps", "PuOr",
                 "RdBu", "RdGy", "PiYG", "PRGn", "BrBG", "RdYlBu", "RdYlGn", "Spectral",
                 "Zissou 1", "Cividis", "Roma")
  grdevpals <- grdevpals[order(grdevpals)]
  grdevpals2 <- c("rainbow", "heat", "terrain", "topo", "cm")

  if(is.na(palette)){
    return(NA)
  }

  if(palette == "all"){
    return(glottocolpal_options())
  }

  if(palette %in% viridispals){
    rlang::check_installed("viridisLite", reason = "to use `glottocolpal()`")
    if(is.null(ncolr)){ncolr <- 20}
    colpal <- viridisLite::viridis(n = ncolr, option = palette, alpha = alpha, direction = {ifelse(rev == FALSE, 1, -1)})
  } else if(palette %in% grdevpals){ # contains palettes from RColorBrewer and viridis. grDevices is more flexible than RColorBrewer and therefore is prioritized
    rlang::check_installed("grDevices", reason = "to use `glottocolpal()`")
    if(is.null(ncolr)){ncolr <- 20}
    colpal <- grDevices::hcl.colors(n = ncolr, palette = palette, rev = rev, alpha = alpha)
  } else if(palette %in% colorbrewerpals){
    rlang::check_installed("RColorBrewer", reason = "to use `glottocolpal()`")
    if(is.null(ncolr)){ncolr <- RColorBrewer::brewer.pal.info[colorbrewerpals == palette, "maxcolors"]}
    colpal <- RColorBrewer::brewer.pal(ncolr, palette)
    if(rev == TRUE){colpal <- rev(colpal)}
  } else if(palette %in% grdevpals2){
    rlang::check_installed("grDevices", reason = "to use `glottocolpal()`")
    if(is.null(ncolr)){ncolr <- 20}
      if(palette == "rainbow"){colpal <- grDevices::rainbow(n = ncolr, rev = rev, alpha = alpha)}
      if(palette == "heat"){colpal <- grDevices::heat.colors(n = ncolr, rev = rev, alpha = alpha)}
      if(palette == "terrain"){colpal <- grDevices::terrain.colors(n = ncolr, rev = rev, alpha = alpha)}
      if(palette == "topo"){colpal <- grDevices::topo.colors(n = ncolr, rev = rev, alpha = alpha)}
      if(palette == "cm"){colpal <- grDevices::cm.colors(n = ncolr, rev = rev, alpha = alpha)}
    }

  if(show == TRUE){
    rlang::check_installed("scales", reason = "if `show == TRUE`")
    scales::show_col(colours = colpal)
  }
  return(colpal)
}

#' Show which color palettes are available
#' @keywords internal
#' @return Overview of color palettes available to choose from.
#' @export
#'
glottocolpal_options <- function(){
  viridispals <- c("viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo")
  # dput(as.character(rownames(RColorBrewer::brewer.pal.info))) # intermediate step to reduce dependencies. Output pasted below
  colorbrewerpals <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
                       "Spectral", "Accent", "Dark2", "Paired", "Pastel1", "Pastel2",
                       "Set1", "Set2", "Set3", "Blues", "BuGn", "BuPu", "GnBu", "Greens",
                       "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples",
                       "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")
  # dput(as.character(grDevices::hcl.pals())) # intermediate step to reduce dependencies. Output pasted below
  grdevpals <- c("Pastel 1", "Dark 2", "Dark 3", "Set 2", "Set 3", "Warm", "Cold",
                 "Harmonic", "Dynamic", "Grays", "Light Grays", "Blues 2", "Blues 3",
                 "Purples 2", "Purples 3", "Reds 2", "Reds 3", "Greens 2", "Greens 3",
                 "Oslo", "Purple-Blue", "Red-Purple", "Red-Blue", "Purple-Orange",
                 "Purple-Yellow", "Blue-Yellow", "Green-Yellow", "Red-Yellow",
                 "Heat", "Heat 2", "Terrain", "Terrain 2", "Viridis", "Plasma",
                 "Inferno", "Rocket", "Mako", "Dark Mint", "Mint", "BluGrn", "Teal",
                 "TealGrn", "Emrld", "BluYl", "ag_GrnYl", "Peach", "PinkYl", "Burg",
                 "BurgYl", "RedOr", "OrYel", "Purp", "PurpOr", "Sunset", "Magenta",
                 "SunsetDark", "ag_Sunset", "BrwnYl", "YlOrRd", "YlOrBr", "OrRd",
                 "Oranges", "YlGn", "YlGnBu", "Reds", "RdPu", "PuRd", "Purples",
                 "PuBuGn", "PuBu", "Greens", "BuGn", "GnBu", "BuPu", "Blues",
                 "Lajolla", "Turku", "Hawaii", "Batlow", "Blue-Red", "Blue-Red 2",
                 "Blue-Red 3", "Red-Green", "Purple-Green", "Purple-Brown", "Green-Brown",
                 "Blue-Yellow 2", "Blue-Yellow 3", "Green-Orange", "Cyan-Magenta",
                 "Tropic", "Broc", "Cork", "Vik", "Berlin", "Lisbon", "Tofino",
                 "ArmyRose", "Earth", "Fall", "Geyser", "TealRose", "Temps", "PuOr",
                 "RdBu", "RdGy", "PiYG", "PRGn", "BrBG", "RdYlBu", "RdYlGn", "Spectral",
                 "Zissou 1", "Cividis", "Roma")
  grdevpals <- grdevpals[order(grdevpals)]
  grdevpals2 <- c("rainbow", "heat", "terrain", "topo", "cm")

  message("Viridis palettes: \n")
  printmessage(viridispals)
  message("\n\n")

  message("grDevices palettes: \n")
  printmessage(grdevpals)
  message("\n\n")

  message("Additional grDevices palettes: \n")
  printmessage(grdevpals2)
  message("\n\n")

  message("ColorBrewer palettes: \n")
  printmessage(colorbrewerpals)
  message("\n\n")
}


#' Highlight certain data points in visualizations
#'
#' This function creates two separate color scales: one for points to highlight,
#' and a second for the remaining background points. It also creates a legend.
#' This is useful for preparing the data for visualizations such as maps or
#' other plots.
#'
#' @param glottodata User-provided glottodata
#' @param spotcol Name of the column that contains the data to put in the spotlights (as well as remaining background data).
#' @param spotlight Selection of data to put in the spotlights.
#' @param spotcontrast Optional column to contrast between data points in the spotlight.
#'
#' @return A glottodata object with columns added to be used in visualization.
#'
#' @export
#'
#' @examples
#' \donttest{
#' glottodata <- glottofilter(country = c("Netherlands", "Germany", "Belgium") )
#' glottodata <- glottospotlight(glottodata = glottodata, spotcol = "country",
#'                               spotlight = "Netherlands")
#' glottomap(glottodata, color = "legend")
#' }
glottospotlight <- function(glottodata, spotcol, spotlight, spotcontrast = NULL){
  # if(is.null(spotpal)){spotpal <- "rainbow"}
  # if(is.null(bgpal)){bgpal <- "Grays"}
  # if(is.null(bgcolr)){bgcolr <- "lightgrey"}
  if(is_sf(glottodata)){
    data <- sf::st_drop_geometry(glottodata)
    was_sf <- TRUE
  } else {
    data <- glottodata
    was_sf <- FALSE
  }

  data$spotlight <- data[,spotcol] %in% spotlight

  data$legend <- rep(NA, nrow(data))

  if(is.null(spotcontrast)){
    data$legend <- ifelse(data$spotlight == TRUE, data[,spotcol], data$legend)
  } else{
    data$legend <- ifelse(data$spotlight == TRUE, data[,spotcontrast], data$legend)
  }

  # spotlightnames <- as.factor(data$legend[data$spotlight == TRUE])
  # ncolrspot <- length(unique(spotlightnames))
  # colpalspot <- glottocolpal(palette = spotpal, ncolrspot)

  # bgnames <- as.factor(data$legend[data$spotlight == FALSE])
  # ncolrbg <- length(unique(bgnames))
  # colpalbg <- glottocolpal(palette = bgpal, ncolr = ncolrbg)

  # data$color[data$spotlight == TRUE] <- colpalspot[spotlightnames]
  # data$color[data$spotlight == FALSE] <- colpalbg[bgnames]

  if(was_sf == TRUE){
    if(glottocheck_isglottodata(data)){
    glottodata <- suppressMessages(dplyr::left_join(glottodata, data[, c("glottocode", "spotlight", "legend")]))
    } else if(glottocheck_isglottosubdata(data)){
      glottodata <- suppressMessages(dplyr::left_join(glottodata, data[, c("glottosubcode", "spotlight", "legend")]))
    }
    return(glottodata)
  } else {
    return(data)
  }
}

glottospotlight_legend <- function(glottodata){
  if(all(c("spotlight", "legend") %in% colnames(glottodata))){
    legend <- list(
      spotlight = TRUE,
      labels = unique(glottodata[sf::st_drop_geometry(glottodata[,"spotlight"]) == TRUE,]$legend)
      # col = unique(glottodata[sf::st_drop_geometry(glottodata[,"spotlight"]) == TRUE,]$color)
      )
  } else {
    legend <- list(spotlight = FALSE)
  }
  legend
}
