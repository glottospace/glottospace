#' Nonmetric Multidimensional Scaling for a glottodist object
#'
#' @param glottodist A glottodist object
#' @param k Number of dimensions. Either 2 or 3 for nmds.
#' @param rm.na Whether na's should be removed (default is FALSE)
#' @param row2id In case of nmds, specify what each row contains (either 'glottocode' or 'glottosubcode')
#'
#' @return a glottonmds object
#' @export
#'
glottonmds <- function(glottodist = NULL, k = NULL, rm.na = FALSE, row2id = NULL){
  if(is.null(k)){stop("Please specify k (number of dimensions)")}
  glottonmds <- glottonmds_run(glottodist = glottodist, k = k, rm.na = rm.na)
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
#' @param rm.na Whether NAs should be removed (default is FALSE)
#' @family <glottoplot>
#'
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
#' glottonmds <- glottonmds_run(glottodist = glottodist, k = 2)
glottonmds_run <- function(glottodist, k = 2, rm.na = FALSE){



  if(rm.na == TRUE){
    distmat <- glottoclean_dist_rmna(glottodist)
  } else{
    distmat <- contransform_distmat(glottodist)
  }

  rlang::check_installed("vegan", reason = "to use `glottonmds_run()`")
  tryCatch(
    expr = {vegan::metaMDS(comm = distmat, k = k)},
    error = function(e){
      message("Failed to create glottoNMDS. This might be because glottodist contains NAs. You might consider dropping all rows and columns with NA by specifying rm.na = TRUE")
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
  scores <- as.data.frame(vegan::scores(glottonmds))
  if(is.null(row2id)){row2id <- "glottocode"}
  scores <- tibble::rownames_to_column(scores, row2id)
  scores
}
