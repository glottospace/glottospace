
#' Visualize glottodata or glottodistances
#'
#' This function offers different types of visualizations for linguistic data and linguistic distances.
#'
#' @param type The type of plot: "heatmap", "nmds", or "missing". Default is heatmap if nothing is provided.
#' @param glottodata glottodata table
#' @param glottodist A dist object created with \code{\link{glottodist}}
#' @param colorby Name of variable to be used to color features (optional). Run glottovars() to see the options.
#' @param label Name of variable to be used to label features (optional). Run glottovars() to see the options.
#' @param ptsize Size of points between 0 and 1 (optional)
#' @param filename Optional filename if output should be saved.
#' @param palette Name of color palette, use glottocolpal("all") to see the options
#' @param glottonmds A glottonmds object created with \code{\link{glottonmds}}
#' @param k Number of dimensions. Either 2 or 3 for nmds.
#' @param rm.na Whether na's should be removed (default is FALSE)
#' @param row2id In case of nmds, specify what each row contains (either 'glottocode' or 'glottosubcode')
#' @param preventoverlap For nmds with 2 dimensions, should overlap between data points be prevented?
#' @param alpha For nmds with 2 dimensions: Transparency of points between 0 (very transparent) and 1 (not transparent)
#' @param colorvec Vector specifying colors for individual values and legend
#'   order (non-matching values are omitted), for example:
#'   c("Arawakan" = "rosybrown1", "Yucuna"  = "red",
#'   "Tucanoan" = "lightskyblue1", "Tanimuca-Retuarã" = "blue", "Naduhup" =
#'   "gray70", "Kakua-Nukak" = "gray30")
#'   See the 'values' argument in ggplot2::scale_color_manual() for details.
#'
#' @return a visualization of a glotto(sub)data, glottodist or glottonmds object, which can be saved with glottosave()
#' @export
#'
#' @examples
#' \donttest{
#' # Plot glottodist as nmds:
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
#' glottoplot(glottodist = glottodist, type = "nmds",
#'  k = 3, colorby = "family", label = "name", row2id = "glottocode")
#'
#' # To create a stress/scree plot, you can run:
#' # goeveg::dimcheckMDS(matrix = as.matrix(glottodist), k = k)
#'
#'
#' # Plot missing data:
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodata <- glottosimplify(glottodata)
#' glottoplot(glottodata = glottodata, type = "missing")
#' }
glottoplot <- function(glottodata = NULL, glottodist = NULL, type = NULL, glottonmds = NULL,
                       colorby = NULL, ptsize = NULL, label = NULL, filename = NULL, palette = NULL,
                       k = NULL, rm.na = FALSE, row2id = NULL,
                       preventoverlap = FALSE, alpha = NULL, colorvec = NULL){

  if(is_dist(glottodata)){
    glottodist <- glottodata
  }
  if(glottocheck_isglottodata(glottodist)){
    glottodata <- glottodist
  }

  if( is.null(type)  ){
    if(is.null(glottonmds)){
    glottoplot_heatmap(glottodist = glottodist, filename = filename)
    }
    if(!is.null(glottonmds)){
    glottoplot_nmds(glottonmds = glottonmds,
                      colorby = colorby, ptsize = ptsize, label = label, palette = palette, filename = filename, preventoverlap = preventoverlap, alpha = alpha)
    }
  }

  if( !is.null(type)){
   if(type == "heatmap"){
     glottoplot_heatmap(glottodist = glottodist, filename = filename)
   }
    if(type == "nmds" & !is.null(glottodist)){
      glottonmds <- glottonmds(glottodist = glottodist, k = k, rm.na = rm.na, row2id = row2id)
      glottoplot_nmds(glottonmds = glottonmds,
                      colorby = colorby, ptsize = ptsize, label = label, palette = palette, filename = filename, preventoverlap = preventoverlap, alpha = alpha)
    }
    if(type == "missing"){
      glottoplot_naviewer(data = glottodata, id = "glottocode")
    }
  }





}



#' Plot nmds in 2d or 3d
#'
#' @param glottonmds An glottonmds object
#' @param colorby Name of column to color by
#' @param ptsize ptsize
#' @param label label
#' @param palette color palette
#' @param preventoverlap Only for 2d plots, Should overlap between data point be prevented?
#' @param filename optional filename if output should be saved.
#' @param alpha Transparency of points between 0 (very transparent) and 1 (not transparent)
#' @param colorvec Vector specifying colors for individual values and legend
#'   order (non-matching values are omitted), for example:
#'   c("Arawakan" = "rosybrown1", "Yucuna"  = "red",
#'   "Tucanoan" = "lightskyblue1", "Tanimuca-Retuarã" = "blue", "Naduhup" =
#'   "gray70", "Kakua-Nukak" = "gray30")
#'   See the 'values' argument in ggplot2::scale_color_manual() for details.
#'
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
#' glottonmds <- glottonmds(glottodist, k = 2, row2id = "glottocode")
#'
#' glottoplot_nmds(glottonmds = glottonmds, colorby = "family", ptsize = "isolate")
#' glottoplot_nmds(glottonmds = glottonmds, colorby = "isolate")
glottoplot_nmds <- function(glottonmds, colorby = NULL, ptsize = NULL, label = NULL, palette = NULL, filename = NULL, preventoverlap = FALSE, alpha = NULL, colorvec = NULL){

  nmds <- glottonmds[[1]]
  scoresdata <- glottonmds[[2]]

  if(nmds$ndim == 2){
    glottoplot_nmds_2d(nmds = nmds, scoresdata = scoresdata, colorby = colorby, ptsize = ptsize, label = label, filename = filename, preventoverlap = preventoverlap, alpha = alpha, colorvec = colorvec)
  }

  if(nmds$ndim == 3){
    glottoplot_nmds_3d(nmds = nmds, scoresdata = scoresdata, colorby = colorby, ptsize = ptsize, label = label, palette = palette, filename = filename)
  }

}

#' Plot nmds in 2d
#'
#' @param nmds An nmds object
#' @param scoresdata scoresdata
#' @param colorby colorby
#' @param ptsize ptsize
#' @param label label
#' @param preventoverlap Should overlap between data points be avoided?
#' @param filename optional filename if output should be saved.
#' @param alpha Transparency of points between 0 (very transparent) and 1 (not
#'   transparent)
#' @param colorvec Vector specifying colors for individual values and legend
#'   order (non-matching values are omitted), for example:
#'   c("Arawakan" = "rosybrown1", "Yucuna"  = "red",
#'   "Tucanoan" = "lightskyblue1", "Tanimuca-Retuarã" = "blue", "Naduhup" =
#'   "gray70", "Kakua-Nukak" = "gray30")
#'   See the 'values' argument in ggplot2::scale_color_manual() for details.
#'
#' @noRd
#'
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
#' nmds <- glottonmds_run(glottodist, k = 2)
#' scores <- glottonmds_scores(nmds)
#' scoresdata <- glottojoin_base(scores)
#'
#' glottoplot_nmds_2d(nmds = nmds, scoresdata = scoresdata, colorby = "family", ptsize = "isolate")
#' glottoplot_nmds_2d(nmds = nmds, scoresdata = scoresdata, colorby = "isolate")
glottoplot_nmds_2d <- function(nmds, scoresdata, colorby = NULL, ptsize = NULL, label = NULL, filename = NULL, alpha = NULL, preventoverlap = FALSE, colorvec = NULL){

  scoresdata <- glottosimplify(scoresdata)
   duplo <- sum(duplicated(scoresdata[,c("NMDS1", "NMDS2")]) | duplicated(scoresdata[,c("NMDS1", "NMDS2")], fromLast = TRUE))
  if(duplo != 0 & preventoverlap == FALSE){
    message(paste0("Due to overlap, not all of the ", nrow(scoresdata), " datapoints are visible \n"))
    message(paste0("You might consider specifying preventoverlap = TRUE \n"))
  }
if(preventoverlap == FALSE){
  if(is.null(alpha)){alpha <- 1}
    nmdsplot <- ggplot2::ggplot(data = scoresdata, ggplot2::aes_string(x="NMDS1",y="NMDS2", col = colorby) ) +
      ggplot2::geom_point(size = ptsize, alpha = alpha) +
      {if(!is.null(label))ggplot2::geom_text(ggplot2::aes_string(label = label), hjust = 0, vjust = 0, show.legend = FALSE)} +
      ggplot2::coord_equal()+
      ggplot2::labs(title = paste0("NMDS (k = ", nmds$ndim, ", stress = ", round(nmds$stress, 2), ")"), x = "NMDS1", y = "NMDS2") +
      {if(!is.null(colorvec)){ggplot2::scale_color_manual(values = colorvec)}} +
      ggplot2::theme(panel.background = ggplot2::element_blank(), legend.key=ggplot2::element_rect(fill="white"))
} else{
  if(is.null(alpha)){alpha <- .3}
  nmdsplot <- ggplot2::ggplot(data = scoresdata, ggplot2::aes_string(x="NMDS1",y="NMDS2", col = colorby) ) +
    ggplot2::geom_point(size = ptsize, alpha = alpha, position = ggplot2::position_jitter(width = 0.01, height = 0.01, seed = 22)) +
    {if(!is.null(label))ggplot2::geom_text(position = ggplot2::position_jitter(width = 0.01, height = 0.01, seed = 22), ggplot2::aes_string(label = label), hjust = 0, vjust = 0, show.legend = FALSE)} +
    ggplot2::coord_equal()+
    ggplot2::labs(title = paste0("NMDS (k = ", nmds$ndim, ", stress = ", round(nmds$stress, 2), ")"), x = "NMDS1", y = "NMDS2") +
    {if(!is.null(colorvec)){ggplot2::scale_color_manual(values = colorvec)}} +
    ggplot2::theme(panel.background = ggplot2::element_blank(), legend.key=ggplot2::element_rect(fill="white"))
}



      if(!is.null(filename)){
        if( tools::file_ext(filename) == "" ){filename <- paste0(filename, ".png")}
        ggplot2::ggsave(plot = nmdsplot, filename = filename)
        }
    print(nmdsplot)

}

#' Plot nmds in 3d
#'
#' @param nmds An nmds object
#' @param scoresdata scoresdata
#' @param colorby color
#' @param ptsize ptsize
#' @param label label
#' @param filename optional filename if output should be saved.
#' @noRd
#'
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
#' nmds <- glottonmds_run(glottodist, k = 3)
#' scores <- glottonmds_scores(nmds)
#' scoresdata <- glottojoin_base(scores)
#'
#' glottoplot_nmds_3d(nmds = nmds, scoresdata = scoresdata, colorby = "family", label = "name")
#' glottoplot_nmds_3d(nmds = nmds, scoresdata = scoresdata, colorby = "isolate")
glottoplot_nmds_3d <- function(nmds, scoresdata, colorby = NULL, ptsize = NULL, label = NULL, palette = NULL, filename = NULL){
  rlang::check_installed("plotly", reason = "to use `glottoplot_nmds_3d()`")
  scoresdata <- glottosimplify(scoresdata)

  if(is.null(colorby)){
    colorby <- "allsame"
    scoresdata$allsame <- "allsame"
  }

  if(is.null(palette)){
    palette = "turbo"
  }

  colpal <- glottocolpal(palette = palette, ncolr = length(unique(scoresdata[[colorby]])) )
  # colpal[as.factor(scoresdata[[colorby]])]

  nmdsplot <- plotly::plot_ly(type="scatter3d", mode="markers", colors = colpal)
  for (i in unique(scoresdata[[colorby]])) {

    nmdsplot <- plotly::add_trace(nmdsplot,
                   data = scoresdata[scoresdata[[colorby]] == i,],
                   legendgroup = i,
                   x = ~NMDS1,
                   y = ~NMDS2,
                   z = ~NMDS3,
                   type = 'scatter3d',
                   mode = 'markers',
                   size = ptsize,
                   color = ~.data[[colorby]],
                   hoverinfo = "text",
                   text = {if(!is.null(label))~.data[[label]]},
                   showlegend = ifelse(colorby == "allsame", FALSE, TRUE))
  }

   nmdsplot <- nmdsplot %>% plotly::layout(
    title = paste0("NMDS (k = ", nmds$ndim, ", stress = ", round(nmds$stress, 2), ")"),
    scene = list(
      xaxis = list(title = "NMDS1"),
      yaxis = list(title = "NMDS2"),
      zaxis = list(title = "NMDS3")
    ) )


  if(!is.null(filename)){
    if( tools::file_ext(filename) == "" ){filename <- paste0(filename, ".html")}
    rlang::check_installed("htmlwidgets", reason = "to save a 3D nmdsplot")
    htmlwidgets::saveWidget(nmdsplot, title = "NMDS 3D", filename)
    }
  print(nmdsplot)
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

  if(!is.null(filename)){
    if( tools::file_ext(filename) == "" ){filename <- paste0(filename, ".png")}
    ggplot2::ggsave(plot = heatmap, filename = filename)
    }
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


