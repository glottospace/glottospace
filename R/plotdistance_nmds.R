# FIXME: 3d plot nmds

#' Nonmetric Multidimensional Scaling
#'
#' @param k
#' @param dist
#' @param rm.na
#'
#' @return
#' @export
#'
#' @examples
#' dist <- glottodist(glottodata = glottodata, structure = structure)
#' nmds <- nmds(dist = dist, k = 2)
nmds <- function(dist, k = 2, rm.na = FALSE){
  if(any(class(dist) == "dist")){
  distmat <- as.matrix(dist)
  } else if (any(class(dist) == "matrix")){
    distmat <- dist
    # dist <- dist(distmat)
  }

  # Remove NAs!
  if(rm.na == TRUE){
    rowna <- rowSums(is.na(distmat))
    colna <- colSums(is.na(distmat))

    rmcol <- which(colSums(is.na(distmat)) > min(colna))
    rmrow <- which(rowSums(is.na(distmat)) > min(rowna))

    if(!purrr::is_empty(rmcol)){  distmat <- distmat[,-rmcol] }
    if(!purrr::is_empty(rmrow)){  distmat <- distmat[-rmrow,] }
  }

  nmds <- vegan::metaMDS(comm = distmat, k = k)
  # Default is to use the monoMDS function in vegan, but also possible to use isoMDS of MASS.
  # If you supply a distance structure to metaMDS, it will be used as such and argument method is ignored.
  # https://github.com/vegandevs/vegan/issues/330

}

#' Title
#'
#' @param nmds
#'
#' @return
#' @export
#'
#' @examples
nmds_scores <- function(nmds){
  scores <- as.data.frame(vegan::scores(nmds))
  scores <- tibble::rownames_to_column(scores, "glottocode")
  scores
}

#' Plot NMDS
#'
#' @param nmds
#' @param filename
#' @param dist
#' @param view
#' @param rm.na
#'
#' @return
#' @export
#'
#' @examples
#' Join NMDS scores with glottodata:
#' nmds <- nmds(glottodata, k = 2)
#' scores <- nmds_scores(nmds)
#' scoresdata <- join_glottodata(data = scores, glottodata = get_glottobase())
#'
#'
#' plotnmds(nmds = nmds, scoresdata = scoresdata, colorby = "family_name", sizeby = "isolate")
#' plotnmds(nmds = nmds, scoresdata = scoresdata, colorby = "isolate")
plotnmds <- function(nmds, scoresdata, colorby = NULL, sizeby = NULL, labelby = NULL, filename = NULL){

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


