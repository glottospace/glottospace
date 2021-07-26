nmds <- function(k = 2, dist, rm.na = TRUE){
  if (!require(purrr)) {install.packages('purrr')}
  library(purrr)

  conmat <- as.matrix(dist)

  # Remove NAs!
  if(rm.na == TRUE){
    rowna <- rowSums(is.na(conmat))
    colna <- colSums(is.na(conmat))

    rmcol <- which(colSums(is.na(conmat)) > min(colna))
    rmrow <- which(rowSums(is.na(conmat)) > min(rowna))

    if(!purrr::is_empty(rmcol)){  conmat <- conmat[,-rmcol] }
    if(!purrr::is_empty(rmrow)){  conmat <- conmat[-rmrow,] }
  }

  nmds <- vegan::metaMDS(comm = conmat, k = k) # Default is to use the monoMDS function in vegan, but lso possible to use isoMDS of MASS.
  # If you supply a distance structure to metaMDS, it will be used as such and argument method is ignored.
  # https://github.com/vegandevs/vegan/issues/330

  nmds_res <- as.data.frame(scores(nmds))
  nmds_res$label <- rownames(nmds_res)
  # nmds_res$language <- sub("\\_.*", "", rownames(nmds_res))
  # nmds_res$family <- ifelse(nmds_res$language %in% tuca, "Tucanoan", "Arawakan")
  # nmds_res$yucutani <- ifelse(nmds_res$language == "tani1257" | nmds_res$language == "yucu1253", "Yucuna-Tanimuka", "Others")
  # nmds_res$ytfamily <- ifelse(nmds_res$language == "tani1257" | nmds_res$language == "yucu1253",
  #                             nmds_res$language, nmds_res$family)
  # nmds_res$groups <- langtab$name[match(nmds_res$ytfamily, langtab$glottocode)]
  # nmds_res$groups <- factor(nmds_res$groups, levels = c("Arawakan", "Yucuna", "Tucanoan", "Tanimuca"))
  list(nmds, nmds_res)
}
