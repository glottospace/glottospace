
#' Visualize distances
#'
#' This function offers different types of visualizations to show linguistic distances.
#'
#' @param type The type of plot: "heatmap", "nmds"
#' @param glottodist A dist object created with \code{\link{glottodist}}
#' @param filename Optional filename if output should be saved.
#' @param ... Additional arguments passed to plotnmds (if type = "nmds").
#'
#' @return
#' @export
#'
#' @examples
#' glottodata <- glottoget_path(meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
glottoplot <- function(glottodist, type = NULL, filename = NULL, ...){
  if(is.null(type)){type <- "heatmap"}

  if(type == "heatmap"){
    glottoplot_heatmap(glottodist = glottodist, filename = filename)
  }

  if(type == "nmds"){

  }
}

#' Nonmetric Multidimensional Scaling
#'
#' @param k Number of dimensions
#' @param dist dist object or distance matrix
#' @param rm.na Whether NAs should be removed (default is FALSE)
#' @keywords internal
#' @family <glottoplot>
#' @return
#' @export
#'
#' @examples
#' glottodata <- glottoget_glottodata()
#' dist <- glottodist(glottodata = glottodata, structure = structure)
#' nmds <- glottonmds(dist = dist, k = 2)
glottonmds <- function(dist, k = 2, rm.na = FALSE){
  distmat <- contransform_distmat(dist)

  # Remove NAs!
  if(rm.na == TRUE){
    rowna <- rowSums(is.na(distmat))
    colna <- colSums(is.na(distmat))

    rmcol <- which(colSums(is.na(distmat)) > min(colna))
    rmrow <- which(rowSums(is.na(distmat)) > min(rowna))

    if(!purrr::is_empty(rmcol)){  distmat <- distmat[,-rmcol] }
    if(!purrr::is_empty(rmrow)){  distmat <- distmat[-rmrow,] }
  }

  vegan::metaMDS(comm = distmat, k = k)
  # Default is to use the monoMDS function in vegan, but also possible to use isoMDS of MASS.
  # If you supply a distance structure to metaMDS, it will be used as such and argument method is ignored.
  # https://github.com/vegandevs/vegan/issues/330

}

#' Get nmds scores
#'
#' Obtain nmds scores of a glottonmds object
#'
#' @param glottonmds a glottonmnds object created with glottonmds_run()
#'
#' @return
#' @export
#' @keywords internal
#' @family <glottoplot>
#'
#' @examples
#' glottonmds_scores(glottonmds)
glottonmds_scores <- function(glottonmds){
  scores <- as.data.frame(vegan::scores(nmds))
  scores <- tibble::rownames_to_column(scores, "glottocode")
  scores
}

#' Plot nmds
#'
#' @param nmds An nmds object
#' @param scoresdata scoresdata
#' @param colorby colorby
#' @param sizeby sizeby
#' @param labelby label
#' @param filename optional filename if output should be saved.
#'
#' @return
#' @export
#'
#' @examples
#' Join nmds scores with glottodata:
#' nmds <- glottonmds(glottodata, k = 2)
#' scores <- glottonmds_scores(nmds)
#' scoresdata <- glottojoin_base(scores)
#'
#' glottoplot_nmds(nmds = nmds, scoresdata = scoresdata, colorby = "family", sizeby = "isolate")
#' glottoplot_nmds(nmds = nmds, scoresdata = scoresdata, colorby = "isolate")
glottoplot_nmds <- function(nmds, scoresdata, colorby = NULL, sizeby = NULL, labelby = NULL, filename = NULL){

  if(nmds$ndim == 2){
    nmdsplot <- ggplot2::ggplot(data = scoresdata, ggplot2::aes_string(x="NMDS1",y="NMDS2", col = colorby, size = sizeby)) +
      ggplot2::geom_point() +
      {if(!is.null(labelby))ggplot2::geom_text(ggplot2::aes_string(label = labelby), hjust = 0, vjust = 0, show.legend = FALSE)} +
      # {if(ellipse)ggplot2::stat_ellipse(type="t", level = 0.95, show.legend = FALSE, alpha = 0.5, size = 0.75, linetype = 2)} +
      ggplot2::coord_equal()+
      ggplot2::labs(title = paste0("NMDS (k = ", nmds$ndim, ", stress = ", round(nmds$stress, 2), ")"), x = "NMDS1", y = "NMDS2") +
      ggplot2::theme_bw()

      if(!is.null(filename)){ggsave(plot = nmdsplot, filename = filename)}

      print(nmdsplot)
    }

  if(nmds$ndim == 3){
    # update function with tilde ~ ... See isolates_anthrodata_V2 and isolates_anthrodata_dplace
    # perhaps add functionality to save as movie:
    # https://r-graphics.org/recipe-miscgraph-3d-animate
    # http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization#export-the-plot-into-an-interactive-html-file
    nmdsplot <- plotly::plot_ly(data = scoresdata, x = ~NMDS1, y = ~NMDS2, z = ~NMDS3,
                                type="scatter3d", mode="markers",
                                color = {if(!is.null(colorby))~.data[[colorby]]},
                                size = {if(!is.null(sizeby))~.data[[sizeby]]},
                                hoverinfo = "text",
                                text = {if(!is.null(labelby))~.data[[labelby]]})

    nmdsplot <- nmdsplot %>% plotly::layout(
      title = paste0("NMDS (k = ", nmds$ndim, ", stress = ", round(nmds$stress, 2), ")"),
      scene = list(
        xaxis = list(title = "NMDS1"),
        yaxis = list(title = "NMDS2"),
        zaxis = list(title = "NMDS3")
      ))

      nmdsplot
      if(!is.null(filename)){htmlwidgets::saveWidget(nmdsplot, title = "NMDS 3D", filename)}
      }
}

# if(view == "scree"){
#   distmat <- as.matrix(dist)
#   goeveg::dimcheckMDS(matrix = distmat)
# }
#
# if(view == "stress"){
#   stressplot(scores) # large scatter around line? Original dissimilarities not well preserved in reduced number of dimensions
# }

#' Plot a heatmap of a distance matrix
#'
#' @param glottodist A dist object created with \code{\link{glottodist}}
#' @param filename Optional filename if output should be saved.
#'
#' @return
#' @export
#'
#' @examples
#' glottoplot_heatmap(glottodist)
glottoplot_heatmap <- function(glottodist, filename = NULL){
  distmat <- contransform_distmat(glottodist)

  distpivot <- distmat %>%
    as.data.frame() %>%
    tibble::rownames_to_column("glottocode_x") %>%
    tidyr::pivot_longer(-c(glottocode_x), names_to = "glottocode_y", values_to = "distance")

  heatmap <- ggplot2::ggplot(data = distpivot, ggplot2::aes(x=glottocode_x, y=glottocode_y, fill=distance) ) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_distiller(palette = "YlOrRd", na.value = "grey50", direction = 1) +
    ggplot2::labs(x = "glottocode", y = "glottocode") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

  if(!is.null(filename)){ggplot2::ggsave(plot = heatmap, filename = filename)}
  print(heatmap)



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
  colpalspot <- rainbow(ncolrspot)

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



