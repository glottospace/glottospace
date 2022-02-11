
#' Visualize glottodata or glottodistances
#'
#' This function offers different types of visualizations for linguistic data and linguistic distances.
#'
#' @param type The type of plot: "heatmap", "nmds", or "stress". Default is heatmap if nothing is provided.
#' @param glottodata glottodata table
#' @param glottodist A dist object created with \code{\link{glottodist}}
#' @param k Number of dimensions. Either 2 or 3 for nmds.
#' @param rm.na Whether na's should be removed (default is FALSE)
#' @param color glottovar to be used to color features (optional). Run glottovars() to see the options.
#' @param label glottovar to be used to label features (optional). Run glottovars() to see the options.
#' @param ptsize Size of points between 0 and 1 (optional)
#' @param filename Optional filename if output should be saved.
#'
#' @return a visualization of a glotto(sub)data or glottodist object, which can be saved with glottosave()
#' @export
#'
#' @examples
#' \donttest{
#' # Plot glottodist as nmds:
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
#' glottoplot(glottodist = glottodist, type = "nmds", k = 3, color = "family", label = "name")
#'
#' # Plot missing data:
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodata <- glottosimplify(glottodata)
#' glottoplot(glottodata = glottodata, type = "missing")
#' }
glottoplot <- function(glottodata = NULL, glottodist = NULL, type = NULL, k = NULL, rm.na = FALSE,
                       color = NULL, ptsize = NULL, label = NULL, filename = NULL){
  if(is.null(type)){type <- "heatmap"}
  if(is_dist(glottodata)){
    glottodist <- glottodata
  }
  if(glottocheck_isglottodata(glottodist)){
    glottodata <- glottodist
  }

  if(type == "heatmap"){
    glottoplot_heatmap(glottodist = glottodist, filename = filename)
  }

  if(type == "nmds"){
    if(is.null(k)){stop("Please specify k (number of dimensions)")}
    glottonmds <- glottonmds(glottodist = glottodist, k = k, rm.na = rm.na)
    scores <- glottonmds_scores(glottonmds)
    scoresdata <- glottojoin_base(scores)
    glottoplot_nmds(nmds = glottonmds, scoresdata = scoresdata,
                    color = color, ptsize = ptsize, label = label, filename = filename)
  }

  if(type == "stress"){
    glottoplot_nmds_stress(glottodist = glottodist, k = k)
  }

  if(type == "missing"){
    glottoplot_naviewer(data = glottodata, id = "glottocode")
  }

}

#' Nonmetric Multidimensional Scaling
#'
#' @param k Number of dimensions
#' @param dist dist object or distance matrix
#' @param rm.na Whether NAs should be removed (default is FALSE)
#' @family <glottoplot>
#'
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
#' glottonmds <- glottonmds(glottodist = glottodist, k = 2)
glottonmds <- function(glottodist, k = 2, rm.na = FALSE){
  distmat <- contransform_distmat(glottodist)

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
#'
#' @noRd
#' @family <glottoplot>
#'
#' @examples
#' \dontrun{
#' glottonmds_scores(glottonmds)
#' }
glottonmds_scores <- function(glottonmds){
  scores <- as.data.frame(vegan::scores(glottonmds))
  scores <- tibble::rownames_to_column(scores, "glottocode")
  scores
}

#' Plot nmds in 3d
#'
#' @param nmds An nmds object
#' @param scoresdata scoresdata
#' @param color color
#' @param ptsize ptsize
#' @param label label
#' @param filename optional filename if output should be saved.
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
#' nmds <- glottonmds(glottodist, k = 2)
#' scores <- glottonmds_scores(nmds)
#' scoresdata <- glottojoin_base(scores)
#'
#' glottoplot_nmds(nmds = nmds, scoresdata = scoresdata, color = "family", ptsize = "isolate")
#' glottoplot_nmds(nmds = nmds, scoresdata = scoresdata, color = "isolate")
glottoplot_nmds <- function(nmds, scoresdata, color = NULL, ptsize = NULL, label = NULL, filename = NULL){

  if(nmds$ndim == 2){
    glottoplot_nmds_2d(nmds = nmds, scoresdata = scoresdata, color = color, ptsize = ptsize, label = label, filename = filename)
  }

  if(nmds$ndim == 3){
    glottoplot_nmds_3d(nmds = nmds, scoresdata = scoresdata, color = color, ptsize = ptsize, label = label, filename = filename)
  }

}

#' Plot nmds in 2d
#'
#' @param nmds An nmds object
#' @param scoresdata scoresdata
#' @param color color
#' @param ptsize ptsize
#' @param label label
#' @param filename optional filename if output should be saved.
#' @noRd
#'
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
#' nmds <- glottonmds(glottodist, k = 2)
#' scores <- glottonmds_scores(nmds)
#' scoresdata <- glottojoin_base(scores)
#'
#' glottoplot_nmds_2d(nmds = nmds, scoresdata = scoresdata, color = "family", ptsize = "isolate")
#' glottoplot_nmds_2d(nmds = nmds, scoresdata = scoresdata, color = "isolate")
glottoplot_nmds_2d <- function(nmds, scoresdata, color = NULL, ptsize = NULL, label = NULL, filename = NULL){

    nmdsplot <- ggplot2::ggplot(data = scoresdata, ggplot2::aes_string(x="NMDS1",y="NMDS2", col = color, size = ptsize)) +
      ggplot2::geom_point() +
      {if(!is.null(label))ggplot2::geom_text(ggplot2::aes_string(label = label), hjust = 0, vjust = 0, show.legend = FALSE)} +
      # {if(ellipse)ggplot2::stat_ellipse(type="t", level = 0.95, show.legend = FALSE, alpha = 0.5, size = 0.75, linetype = 2)} +
      ggplot2::coord_equal()+
      ggplot2::labs(title = paste0("NMDS (k = ", nmds$ndim, ", stress = ", round(nmds$stress, 2), ")"), x = "NMDS1", y = "NMDS2") +
      ggplot2::theme_bw()

      if(!is.null(filename)){ggplot2::ggsave(plot = nmdsplot, filename = filename)}

      print(nmdsplot)

}

#' Plot nmds in 3d
#'
#' @param nmds An nmds object
#' @param scoresdata scoresdata
#' @param color color
#' @param ptsize ptsize
#' @param label label
#' @param filename optional filename if output should be saved.
#' @noRd
#'
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
#' nmds <- glottonmds(glottodist, k = 3)
#' scores <- glottonmds_scores(nmds)
#' scoresdata <- glottojoin_base(scores)
#'
#' glottoplot_nmds_3d(nmds = nmds, scoresdata = scoresdata, color = "family", label = "name")
#' glottoplot_nmds_3d(nmds = nmds, scoresdata = scoresdata, color = "isolate")
glottoplot_nmds_3d <- function(nmds, scoresdata, color = NULL, ptsize = NULL, label = NULL, filename = NULL){
  if(is.null(color)){
    color <- "allsame"
    scoresdata$allsame <- "allsame"
  }

  nmdsplot <- plotly::plot_ly(type="scatter3d", mode="markers")
  for (i in unique(scoresdata[[color]])) {

    nmdsplot <- plotly::add_trace(nmdsplot,
                   data = scoresdata[scoresdata[[color]] == i,],
                   legendgroup = i,
                   x = ~NMDS1,
                   y = ~NMDS2,
                   z = ~NMDS3,
                   type = 'scatter3d',
                   mode = 'markers',
                   size = ptsize,
                   color = ~.data[[color]],
                   hoverinfo = "text",
                   text = {if(!is.null(label))~.data[[label]]},
                   showlegend = ifelse(color == "allsame", FALSE, TRUE))
  }

   nmdsplot <- nmdsplot %>% plotly::layout(
    title = paste0("NMDS (k = ", nmds$ndim, ", stress = ", round(nmds$stress, 2), ")"),
    scene = list(
      xaxis = list(title = "NMDS1"),
      yaxis = list(title = "NMDS2"),
      zaxis = list(title = "NMDS3")
    ) )


  if(!is.null(filename)){htmlwidgets::saveWidget(nmdsplot, title = "NMDS 3D", filename)}
  print(nmdsplot)
}


glottoplot_nmds_stress <- function(glottodist, k = NULL){
  if(is.null(k)){k <- 6}
  distmat <- as.matrix(glottodist)
  goeveg::dimcheckMDS(matrix = distmat, k = k)
}

#' Plot a heatmap of a distance matrix
#'
#' @param glottodist A dist object created with \code{\link{glottodist}}
#' @param filename Optional filename if output should be saved.
#'
#'
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata)
#' glottoplot_heatmap(glottodist)
glottoplot_heatmap <- function(glottodist, filename = NULL){
  distmat <- contransform_distmat(glottodist)

  distpivot <- distmat %>%
    as.data.frame() %>%
    tibble::rownames_to_column("glottocode_x") %>%
    tidyr::pivot_longer(-c(.data$glottocode_x), names_to = "glottocode_y", values_to = "distance")

  heatmap <- ggplot2::ggplot(data = distpivot, ggplot2::aes(x=.data$glottocode_x, y=.data$glottocode_y, fill=.data$distance) ) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_distiller(palette = "YlOrRd", na.value = "grey50", direction = 1) +
    ggplot2::labs(x = "glottocode", y = "glottocode") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

  if(!is.null(filename)){ggplot2::ggsave(plot = heatmap, filename = filename)}
  print(heatmap)



}

#' Show data coverage (view NAs)
#'
#' This function plots the NAs in a dataset. If you used another coding to
#' specify missing data, you should run \code{cleandata_recodemissing} or \code{cleanglottodata} first. If you'd
#' like some more advanced ways of handling NAs, you might check out the
#' \code{naniar} package.
#'
#' @param data Any dataset
#' @param id column name with IDs
#' @param rm.na Whether rows without id should be removed.
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodata <- glottoclean(glottodata = glottodata)
#' data <- glottodata[[1]]
#' glottoplot_naviewer(data, id = "glottocode")
glottoplot_naviewer <- function(data, id = NULL, rm.na = TRUE){
  data <- as.data.frame(data)
  if(rm.na == TRUE){data <- data[!is.na(data[[id]]), ]}
  if(!is.null(id)){
    datamissing <- data[,colnames(data) != id ]
  } else {
    datamissing <- data
  }
  datamissing[is.na(datamissing)] <- "nodata"

  datamissing[datamissing != "nodata" ] <- "data"
  datamissing[datamissing == "nodata" ] <- "NA"


  datamissing <- as.matrix(sapply(datamissing, as.character))
  rownames(datamissing) <- data[,id]

  datamissing <- datamissing %>%
    as.data.frame() %>%
    tibble::rownames_to_column(id) %>%
    tidyr::pivot_longer(-c(id), names_to = "variable", values_to = "coverage")

  plotmissing <- ggplot2::ggplot(data = datamissing, ggplot2::aes_string(x="variable", y=id, fill="coverage") ) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual(labels = c("data", "NA"), values = c("navy", "darkred"))

  print(plotmissing)
}


