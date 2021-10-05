# LANGDIST - LINGUISTIC DISTANCES -----------------------------------------



#' Calculate distances between languages
#'
#' @param glottodata A glottodata table, or a list with a glottodata table and a structure table.
#' @param structure If glottodata is a table without metadata, a structure table should be provided. You can create it with glottocreate_structuretable() and add it with glottodata_addtable()
#'
#' @return object of class \code{dist}
#' @export
#'
#' @examples
#' glottodata <- glottoget_path(meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
glottodist <- function(glottodata, structure = NULL){
  if(glottocheck_hasmeta(glottodata) & is.null(structure)){
    structure <- glottodata[["structure"]]
    glottodata <- glottodata[["glottodata"]]
  }

  # glottodata:
  glottodata <- tibble::column_to_rownames(glottodata, "glottocode")

  # structure:
  if(!("varname" %in% colnames(structure) ) ){
    colnames(structure)[1] <- "varname"
    message("The structure table does not contain a 'varname' column, trying with the first column instead.")
  }
  # glottodata <-  glottodata[, (colnames(glottodata) %in% structure$varname)]

  structure <- suppressMessages(dplyr::left_join(data.frame("varname" = colnames(glottodata)), structure))

  # type
  if(!("type" %in% colnames(structure) ) ){
    stop('No type column found in structure. Please add a type column.')
  }

  dropvars <- which(structure$type %nin% glottocreate_lookuptable()$type_lookup )
  if(!purrr::is_empty(dropvars)){
    dropvarnames <- paste0(colnames(glottodata[,dropvars]), collapse = ",")
    message(paste0("The following variables are ignored in distance calculation (their type is not one of the pre-specified types): \n", dropvarnames))
    glottodata <- glottodata[,-dropvars]
    structure <- structure[-dropvars, ]
  }

  symm <- which(structure$type == "symm")
  asymm <- which(structure$type == "asymm")
  numer <- which(structure$type == "numeric")
  fact <- which(structure$type == "factor")
  ordfact <- which(structure$type == "ordered")
  ordratio <- which(structure$type == "ordratio")
  logratio <- which(structure$type == "logratio")

  # levels
  levels <- structure$levels

  # set type
  cbinary <- c(symm, asymm)
  glottodata[cbinary] <- lapply(glottodata[cbinary], as.logical)
  glottodata[numer] <- lapply(glottodata[numer], as.numeric)
  glottodata[fact] <- lapply(glottodata[fact], as.factor)
  glottodata[ordfact] <- mapply(FUN = as.ordfact, x = glottodata[ordfact], levels = levels[ordfact])
  glottodata[ordratio] <- lapply(glottodata[ordratio], as.numeric)
  glottodata[logratio] <- lapply(glottodata[logratio], as.numeric)



  # weights
  if(all(is.na(structure$weight))){
        weights <- rep(1, nrow(structure))
        message('All weights are NA. Default is to weight all variables equally: all weights set to 1')
  } else{
  weights <- structure$weight
  }



  cluster::daisy(x = glottodata, metric = "gower",
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
