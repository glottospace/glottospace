# LANGDIST - LINGUISTIC DISTANCES -----------------------------------------



#' Calculate distances between glots
#'
#' @param data Data frame. Rows can either be glots, or subglots (lower-level aspects of a language, e.g. constructions).
#' Columns contain the variables based on which distances are calculated.
#' @param types Character vector with the same length as the number of columns specifying the type of each column
#' @param levels Character vector with the same length as the number of columns specifying the levels of each column
#' @param weights Character vector with the same length as the number of columns specifying the weight of each column
#' @param structure Data frame specifying per column in the data (optional): types, levels weights. Columns should be named as follows: colnames, type, weight.
#'
#' @return object of class \code{dist}
#' @export
#'
#' @examples
#' glottodist <- glottodist(glottodata = isolates, structure = structure)
glottodist <- function(glottodata, types = NULL, levels = NULL, weights = NULL, structure = NULL){
  # FIXME: currently colnames is hardcoded, perhaps it should be the first column by default, unless a name is provided. Create clear error message to indicate this.
  # Specify column types and levels
  if(!is.null(structure)){
    structure <- suppressMessages(dplyr::left_join(data.frame("colnames" = colnames(glottodata)), structure))
    if(is.null(types)){types <- structure$type}
    if(is.null(levels)){levels <- structure$levels}
    if(is.null(weights)){
      if(is.null(structure$weight)){weights <- rep(1, ncol(glottodata))}
      if(!is.null(structure$weight)){weights <- structure$weight}
    }
  }

  if(!is.null(types)){
    symm <- which(types == "symm")
    asymm <- which(types == "asymm")
    numer <- which(types == "numeric")
    fact <- which(types == "factor")
    ordfact <- which(types == "ordered")
    ordratio <- which(types == "ordratio")
    logratio <- which(types == "logratio")
  } else{message('No type column found in structure. Please add a column labelled type.')}

  # TODO: Check whether pre-specified levels match existing levels

  # set types
  cbinary <- c(symm, asymm)
  glottodata[cbinary] <- lapply(glottodata[cbinary], as.logical)
  glottodata[numer] <- lapply(glottodata[numer], as.numeric)
  glottodata[fact] <- lapply(glottodata[fact], as.factor)
  glottodata[ordfact] <- mapply(FUN = as.ordfact, x = glottodata[ordfact], levels = levels[ordfact])
  glottodata[ordratio] <- lapply(glottodata[ordratio], as.numeric)
  glottodata[logratio] <- lapply(glottodata[logratio], as.numeric)

  dist <- cluster::daisy(x = glottodata, metric = "gower",
                         type = list(symm = symm, asymm = asymm, ordratio = ordratio, logratio = logratio),
                         weights = weights)
}

gs_langdist <- function(data = NULL, index = "constructions", glottocodes = NULL, aggregate = "mean", structure = NULL){



  # Calculate distance matrices
  if(index == "languages"){
    #
  }

  if(index == "threshold"){
    #
  }

  outmatlang <- round(outmatlang, 3)
  return(outmatlang)
}

gs_condistagg <- function(condist, glottocodes, aggregation){
  #' Aggregate distances between constructions per language
  #' #' @param aggregate One of c('mean', 'min', 'sum') indicating how distances should be aggregated to language level.('min' returns best match)

  if(any(class(condist) == "dist")){
    distmat <- as.matrix(condist)
  }
  if(is.matrix(condist)){
    distmat <- condist
  }

  outmatlang <- gs_langmat(glottocodes = glottocodes)

  for (i in seq_along(glottocodes)) {
    for (j in seq_along(glottocodes)) {
      if(glottocodes[i]!=glottocodes[j]) {

        # Subset distance matrix for language a and b
        a <- str_detect(rownames(distmat), glottocodes[i])
        b <- str_detect(rownames(distmat), glottocodes[j])
        distmat_ab <- distmat[a,b, drop = F]

        if(!all(is.na(distmat_ab))){
          # Average of row wise aggregated values
          avgrow <- mean(apply(distmat_ab, 1, FUN=aggregation, na.rm = TRUE))
          # TO DO: median, range, IQR, etc.
          # Average of col wise aggregated values
          avgcol <- mean(apply(distmat_ab, 2, FUN=aggregation, na.rm = TRUE))
          # TO DO: output avgrow and avgcol (distance from perspective of lang a and b is not the same)
          out_ab <- (avgrow + avgcol)/2
        }

        if(all(is.na(distmat_ab))){
          # Only NA in both groups
          out_ab <- NA
        }

        outmatlang[i,j] <- out_ab
      } else if(glottocodes[i]==glottocodes[j]) {
        outmatlang[glottocodes[i],glottocodes[j]] <- 0}
    }
  }
  cat('See you later, aggregator!')
  return(outmatlang)
}

gs_condistconvert <- function(condist, glottocodes, groups = NULL, thresval = NULL, threstype = "absolute"){
  #' Convert distances between constructions per language
  #' @param groups Vector of two groups (e.g. language families) for which average should be calculated.
  #' Default is to use all pairwise distances to calculate average.
  #'
  distmat <- as.matrix(condist)

  if(is.null(groups) & is.null(thresval)){
    thr <- mean(dist)
    cat(paste0('Mean between all constructions is: ', round(thr, 3)))
  }
  if(!is.null(groups) & is.null(thresval)){
    if(length(unique(groups)) >2){stop('Maximum number of groups is 2')}
    group1 <- glottocodes[which(groups %in% unique(groups)[1])]
    group2 <- glottocodes[which(groups %in% unique(groups)[2])]

    groups <- sub("\\_.*", "", names(condist))
    a <- groups %in% group1
    b <- groups %in% group2
    distmat_ab <- distmat[a,b, drop = F]
    thr <- mean(distmat_ab)
    cat(paste0('Mean between groups is: ', round(thr, 3)))
  }
  if(!is.null(thresval) & is.null(groups)){
    thr <- thresval
  }
  if(!is.null(thresval) & !is.null(groups)){
    message('Provide either thresval or groups, not both')
  }

  if(threstype == "absolute"){
    cat('\n +1 = more dissimilar than average, \n -1 less dissimilar than average')
    distmat <- ifelse(distmat > thr, 1, -1)
    return(distmat)
  }

  if(threstype == "center"){
    cat('\n Positive values = more dissimilar than average, \n Negative values = less dissimilar than average')
    distmat <- distmat - thr # equivalent: scale(x = distmat, center = rep(thr, ncol(distmat)), scale = F)
    return(distmat)
  }

}
