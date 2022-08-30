
#' Visualize glottodata or glottodistances
#'
#' This function offers different types of visualizations for linguistic data and linguistic distances.
#'
#' @param type The type of plot: "heatmap", "nmds", or "missing". Default is heatmap if nothing is provided.
#' @param glottodata glottodata table
#' @param glottodist A dist object created with \code{\link{glottodist}}
#' @param color Name of variable to be used to color features (optional). See 'Details' below.
#' @param label Name of variable to be used to label features (optional). See 'Details' below.
#' @param ptsize Size of points between 0 and 1 (optional)
#' @param filename Optional filename if output should be saved.
#' @param palette Name of color palette, use glottocolpal("all") to see the options
#' @param glottonmds A glottonmds object created with \code{\link{glottonmds}}
#' @param k Number of dimensions. Either 2 or 3 for nmds.
#' @param na.rm Whether na's should be removed (default is FALSE)
#' @param row2id In case of nmds, specify what each row contains (either 'glottocode' or 'glottosubcode')
#' @param preventoverlap For nmds with 2 dimensions, should overlap between data points be prevented?
#' @param alpha For nmds with 2 dimensions: Transparency of points between 0 (very transparent) and 1 (not transparent)
#' @param colorvec Vector specifying colors for individual values and legend
#'   order (non-matching values are omitted), for example:
#'   c("Arawakan" = "rosybrown1", "Yucuna"  = "red",
#'   "Tucanoan" = "lightskyblue1", "Tanimuca-Retuarã" = "blue", "Naduhup" =
#'   "gray70", "Kakua-Nukak" = "gray30")
#' @param expand Optionally expand one or all of the axes. Default is
#'   c(0,0,0,0), referring to respectively xmin, xmax, ymin, ymax. If you want
#'   to change the maximum of the x-axis, you would do: c(0,1,0,0).
#' @param lbsize Label size (optional)
#' @param ptshift (optional) If preventoverlap is TRUE, how much should points be shifted?
#' @param lbshift (optional) If preventoverlap is TRUE, how much should labels be shifted?
#'   See the 'values' argument in ggplot2::scale_color_manual() for details.
#' @evalRd glottovars()
#' @return a visualization of a glotto(sub)data, glottodist or glottonmds object, which can be saved with glottosave()
#' @export
#'
#' @examples
#' \donttest{
#' # Plot glottodist as nmds:
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
#' # glottoplot(glottodist = glottodist, type = "nmds",
#' #  k = 2, color = "family", label = "name", row2id = "glottocode")
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
                       color = NULL, ptsize = NULL, label = NULL, filename = NULL, palette = NULL,
                       k = NULL, na.rm = FALSE, row2id = NULL,
                       preventoverlap = FALSE, alpha = NULL, colorvec = NULL, expand = NULL, lbsize = NULL, ptshift = NULL, lbshift = NULL){

  if(is_dist(glottodata)){
    glottodist <- glottodata
  }
  if(glottocheck_isglottodata(glottodist)){
    glottodata <- glottodist
  }

  if(!is.numeric(ptsize) & !is.null(ptsize)){stop("ptsize should be a number between 0 and 1")}

  if( is.null(type)  ){
    if(is.null(glottonmds)){
    glottoplot_heatmap(glottodist = glottodist, filename = filename)
    }
    if(!is.null(glottonmds)){
    glottoplot_nmds(glottonmds = glottonmds,
                      color = color, ptsize = ptsize, label = label, palette = palette, filename = filename, preventoverlap = preventoverlap, alpha = alpha, colorvec = colorvec, expand = expand, lbsize = lbsize, ptshift = ptshift, lbshift = lbshift)
    }
  }

  if( !is.null(type)){
   if(type == "heatmap"){
     glottoplot_heatmap(glottodist = glottodist, filename = filename)
   }
    if(type == "nmds" & !is.null(glottodist)){
      glottonmds <- glottonmds(glottodist = glottodist, k = k, na.rm = na.rm, row2id = row2id)
      glottoplot_nmds(glottonmds = glottonmds,
                      color = color, ptsize = ptsize, label = label, palette = palette, filename = filename, preventoverlap = preventoverlap, alpha = alpha, colorvec = colorvec, expand = expand, lbsize = lbsize, ptshift = ptshift, lbshift = lbshift)
    }
    if(type == "missing"){
      glottoplot_naviewer(data = glottodata, id = "glottocode")
    }
  }





}



#' Plot nmds in 2d or 3d
#'
#' @param glottonmds An glottonmds object
#' @param color Name of column to color by
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
#' @param expand Optionally expand one or all of the axes. Default is
#'   c(0,0,0,0), referring to respectively xmin, xmax, ymin, ymax. If you want
#'   to change the maximum of the x-axis, you would do: c(0,1,0,0).
#' @param lbsize Label size (optional)
#' @param ptshift (optional) If preventoverlap is TRUE, how much should points be shifted?
#' @param lbshift (optional) If preventoverlap is TRUE, how much should labels be shifted?
#'
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
#' glottonmds <- glottonmds(glottodist, k = 2, row2id = "glottocode")
#'
#' glottoplot_nmds(glottonmds = glottonmds, color = "family", label = "name")
#' glottoplot_nmds(glottonmds = glottonmds, color = "isolate")
glottoplot_nmds <- function(glottonmds, color = NULL, ptsize = NULL, label = NULL, palette = NULL, filename = NULL, preventoverlap = FALSE, alpha = NULL, colorvec = NULL, expand = NULL, lbsize = NULL, ptshift = NULL, lbshift = NULL){

  nmds <- glottonmds$nmds
  scoresdata <- glottonmds$scoresdata

  if(nmds$ndim == 2){
    glottoplot_nmds_2d(nmds = nmds, scoresdata = scoresdata, color = color, ptsize = ptsize, label = label, filename = filename, preventoverlap = preventoverlap, alpha = alpha, colorvec = colorvec, expand = expand, lbsize = lbsize, ptshift = ptshift, lbshift = lbshift)
  }

  if(nmds$ndim == 3){
    glottoplot_nmds_3d(nmds = nmds, scoresdata = scoresdata, color = color, ptsize = ptsize, label = label, palette = palette, filename = filename, colorvec = colorvec)
  }

}

#' Plot nmds in 2d
#'
#' @param nmds An nmds object
#' @param scoresdata scoresdata
#' @param color color
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
#' @param expand Optionally expand one or all of the axes. Default is
#'   c(0,0,0,0), referring to respectively xmin, xmax, ymin, ymax. If you want
#'   to change the maximum of the x-axis, you would do: c(0,1,0,0).
#' @param lbsize Label size (optional)
#' @param ptshift (optional) If preventoverlap is TRUE, how much should points be shifted?
#' @param lbshift (optional) If preventoverlap is TRUE, how much should labels be shifted?
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
#' glottoplot_nmds_2d(nmds = nmds, scoresdata = scoresdata, color = "family")
glottoplot_nmds_2d <- function(nmds, scoresdata, color = NULL, ptsize = NULL, label = NULL, filename = NULL, alpha = NULL, preventoverlap = FALSE, colorvec = NULL, expand = NULL, lbsize = NULL, ptshift = NULL, lbshift = NULL){

  if(is.null(expand)){expand <- c(0,0,0,0)}

  scoresdata <- glottosimplify(scoresdata)
   duplo <- sum(duplicated(scoresdata[,c("NMDS1", "NMDS2")]) | duplicated(scoresdata[,c("NMDS1", "NMDS2")], fromLast = TRUE))
  if(duplo != 0 & preventoverlap == FALSE){
    message(paste0("Due to overlap, ", duplo, " of the ", nrow(scoresdata), " datapoints are not visible \n"))
    message(paste0("You might consider specifying preventoverlap = TRUE \n"))
  }

   if(is.null(ptsize)){ptsize <- 2}
   if(is.null(lbsize)){lbsize <- 3}
if(preventoverlap == FALSE){
  if(is.null(alpha)){alpha <- 1}
  nmdsplot <- ggplot2::ggplot(data = scoresdata, ggplot2::aes_string(x="NMDS1",y="NMDS2", col = color) ) +
      ggplot2::geom_point(size = ptsize, alpha = alpha) +
      {if(!is.null(label))ggplot2::geom_text(ggplot2::aes_string(label = label), size = lbsize, show.legend = FALSE)} +
      ggplot2::coord_equal()+
      ggplot2::labs(title = paste0("NMDS (k = ", nmds$ndim, ", stress = ", round(nmds$stress, 2), ")"), x = "NMDS1", y = "NMDS2") +
      {if(!is.null(colorvec)){ggplot2::scale_color_manual(values = colorvec)}} +
      ggplot2::theme(panel.background = ggplot2::element_blank(), legend.key=ggplot2::element_rect(fill="white")) +
      {if(!is.null(expand)){ggplot2::expand_limits(x = c(expand[1],expand[2]), y = c(expand[3],expand[4]))}}
} else{
  if(is.null(alpha)){alpha <- .3}
  if(is.null(ptshift)){ptshift <- 0.01}
  if(is.null(ptshift)){ptshift <- 0.01}
  nmdsplot <- ggplot2::ggplot(data = scoresdata, ggplot2::aes_string(x="NMDS1",y="NMDS2", col = color) ) +
    ggplot2::geom_point(size = ptsize, alpha = alpha, position = ggplot2::position_jitter(width = ptshift, height = ptshift, seed = 22)) +
    {if(!is.null(label))ggplot2::geom_text(position = ggplot2::position_jitter(width = lbshift, height = lbshift, seed = 22), ggplot2::aes_string(label = label), size = lbsize, show.legend = FALSE)} +
    ggplot2::coord_equal()+
    ggplot2::labs(title = paste0("NMDS (k = ", nmds$ndim, ", stress = ", round(nmds$stress, 2), ")"), x = "NMDS1", y = "NMDS2") +
    {if(!is.null(colorvec)){ggplot2::scale_color_manual(values = colorvec)}} +
    ggplot2::theme(panel.background = ggplot2::element_blank(), legend.key=ggplot2::element_rect(fill="white")) +
    {if(!is.null(expand)){ggplot2::expand_limits(x = c(expand[1],expand[2]), y = c(expand[3],expand[4]))}}
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
#' @param color color
#' @param ptsize ptsize
#' @param label label
#' @param palette optional palette
#' @param colorvec Optional vector specifying colors for individual values and legend
#'   order (non-matching values are omitted), for example:
#'   c("Arawakan" = "rosybrown1", "Yucuna"  = "red",
#'   "Tucanoan" = "lightskyblue1", "Tanimuca-Retuarã" = "blue", "Naduhup" =
#'   "gray70", "Kakua-Nukak" = "gray30")
#' @param filename optional filename if output should be saved.
#'
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
#' glottoplot_nmds_3d(nmds = nmds, scoresdata = scoresdata, color = "family", label = "name")
#' glottoplot_nmds_3d(nmds = nmds, scoresdata = scoresdata, color = "isolate")
glottoplot_nmds_3d <- function(nmds, scoresdata, color = NULL, ptsize = 2, label = NULL, palette = NULL, filename = NULL, colorvec = NULL){
  rlang::check_installed("plotly", reason = "to use `glottoplot_nmds_3d()`")
  scoresdata <- glottosimplify(scoresdata)

  if(is.null(color)){
    color <- "allsame"
    scoresdata$allsame <- "allsame"
  }

  if(is.null(ptsize)){ptsize <- 2}

  if(is.null(palette)){
    palette = "turbo"
  }

  if(is.null(colorvec)){
  colpal <- glottocolpal(palette = palette, ncolr = length(unique(scoresdata[[color]])) )
  } else{
    colpal <- colorvec
  }
  # colpal[as.factor(scoresdata[[color]])]

  nmdsplot <- plotly::plot_ly(type="scatter3d", mode="markers", colors = colpal)
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
#' @param na.rm Whether rows without id should be removed.
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodata <- glottoclean(glottodata = glottodata)
#' data <- glottodata[[1]]
#' glottoplot_naviewer(data, id = "glottocode")
glottoplot_naviewer <- function(data, id = NULL, na.rm = TRUE){
  data <- as.data.frame(data)
  if(na.rm == TRUE){data <- data[!is.na(data[[id]]), ]}
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


