nmds <- function(k = 2, dist, rm.na = FALSE){
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

nmds_scores <- function(nmds){
  scores <- as.data.frame(vegan::scores(nmds))
  scores <- tibble::rownames_to_column(scores, "glottocode")
  scores
}

#' Title
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
#' scores <- nmds_scores(nmds)
#' glottofilter(glottodata = glottobase(), glottocode = scores$glottocode)
viewnmds <- function(nmds, filename = NULL, dist = NULL, view = "nmds", rm.na = TRUE){

  scores <- nmds_scores(nmds)

  if(nmds$ndim == 2){
      p <- ggplot(data = scores, aes(colour = groups)) +
        stat_ellipse(type="t", aes(x=NMDS1,y=NMDS2),level = 0.95, show.legend = F, alpha = 0.5, size = 0.75, linetype = 2) +
        geom_point(aes(x=NMDS1,y=NMDS2,size = groups), alpha = 0.55) + # add the point markers
        scale_colour_manual(values=c("Arawakan" = "tomato1", "Yucuna" = "red3", "Tucanoan" = "royalblue", "Tanimuca" = "navy")) +
        scale_size_manual(values=c(2,5,2,5)) +
        coord_equal() +
        labs(title = paste0("NMDS (k = ", nmds$ndim, ", stress = ", round(nmds$stress, 2), ")"), x = "NMDS1", y = "NMDS2") +
        theme_bw()

      if(!is.null(filename)){ggsave(plot = p, filename = filename)}

      p
    }

  if(nmds$ndim == 3){
      nmdsplot <- plot_ly(data = scores, x = ~NMDS1, y = ~NMDS2, z = ~NMDS3,
                          type="scatter3d", mode="markers", color = ~groups,
                          colors = c("Arawakan" = "tomato1", "Yucuna" = "red3",
                                     "Tucanoan" = "royalblue", "Tanimuca" = "navy"), hoverinfo = "text",
                          text = rownames(scores))

      nmdsplot <- nmdsplot %>% layout(
        title = paste0("NMDS (k = ", nmds$ndim, ", stress = ", round(nmds$stress, 2), ")"),
        scene = list(
          xaxis = list(title = "NMDS1"),
          yaxis = list(title = "NMDS2"),
          zaxis = list(title = "NMDS3")
        ))



      nmdsplot
      saveWidget(nmdsplot, title = "SAPPHIRE - NMDS TAME 3d", "nmdsplot.html")      }





}

# if(view == "scree"){
#   distmat <- as.matrix(dist)
#   goeveg::dimcheckMDS(matrix = distmat)
# }
#
# if(view == "stress"){
#   stressplot(scores) # large scatter around line? Original dissimilarities not well preserved in reduced number of dimensions
# }

