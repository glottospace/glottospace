#' Nonmetric Multidimensional Scaling for a glottodist object
#'
#' This is a wrapper around the monoMDS function in the vegan package.
#'
#' @param glottodist A glottodist object
#' @param k Number of dimensions. Either 2 or 3 for nmds.
#' @param na.rm Whether na's should be removed (default is FALSE)
#' @param row2id In case of nmds, specify what each row contains (either 'glottocode' or 'glottosubcode')
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
#' glottonmds <- glottonmds(glottodist, k = 2, row2id = "glottocode")
#' glottoplot(glottonmds = glottonmds)
#'
#' @return a glottonmds object which can be plotted using glottoplot(glottonmds = ). See ?monoMDS for more details.
#' @export
#'
glottonmds <- function(glottodist = NULL, k = NULL, na.rm = FALSE, row2id = NULL){
  if(is.null(k)){stop("Please specify k (number of dimensions)")}
  glottonmds <- glottonmds_run(glottodist = glottodist, k = k, na.rm = na.rm)
  if(is.null(row2id)){stop("Please specify row2id ('glottocode' or 'glottosubcode')")}
  scores <- glottonmds_scores(glottonmds, row2id = row2id)
  if(row2id == "glottosubcode"){
    scores$glottocode <- glottoconvert_subcodes(scores$glottosubcode)
  }
  scoresdata <- glottojoin_base(scores)
list("nmds" = glottonmds, "scoresdata" = scoresdata)

}

#' Nonmetric Multidimensional Scaling
#'
#' @param k Number of dimensions
#' @param dist dist object or distance matrix
#' @param na.rm Whether NAs should be removed (default is FALSE)
#' @family <glottoplot>
#'
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
#' glottonmds <- glottonmds_run(glottodist = glottodist, k = 2)
glottonmds_run <- function(glottodist, k = 2, na.rm = FALSE){



  if(na.rm == TRUE){
    distmat <- glottoclean_dist_rmna(glottodist)
  } else{
    distmat <- contransform_distmat(glottodist)
  }

  rlang::check_installed("vegan", reason = "to use `glottonmds_run()`")
  tryCatch(
    expr = {vegan::metaMDS(comm = distmat, k = k)},
    error = function(e){
      message("Failed to create glottoNMDS. This might be because glottodist contains NAs. You might consider dropping all rows and columns with NA by specifying na.rm = TRUE")
      printmessage(e)
    }
  )

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
#' @param row2id Name of column where ids should be placed, default is 'glottocode' (can also be 'glottosubcode')
#'
#' @noRd
#' @family <glottoplot>
#'
#' @examples
#' \dontrun{
#' glottonmds_scores(glottonmds)
#' }
glottonmds_scores <- function(glottonmds, row2id = NULL){
  rlang::check_installed("vegan", reason = "to use `glottonmds_scores()`")
  if(all(is.na(glottonmds$species))){glottonmds$species <- NULL} # added to fix issue with versioning.

  scores <- as.data.frame(vegan::scores(glottonmds))
  if(is.null(row2id)){row2id <- "glottocode"}
  scores <- tibble::rownames_to_column(scores, row2id)
  scores
}
