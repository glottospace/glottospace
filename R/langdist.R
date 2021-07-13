# LANGDIST - LINGUISTIC DISTANCES -----------------------------------------





gs_mergedatadist <- function(data, dist, rm.na = TRUE){

  distmat <- as.matrix(dist)

  if(rm.na == TRUE){
    rowna <- rowSums(is.na(distmat))
    colna <- colSums(is.na(distmat))

    rmcol <- which(colSums(is.na(distmat)) > min(colna))
    rmrow <- which(rowSums(is.na(distmat)) > min(rowna))

    if(!purrr::is_empty(rmcol)){  distmat <- distmat[,-rmcol] }
    if(!purrr::is_empty(rmrow)){  distmat <- distmat[-rmrow,] }
  }

  distdf <- as.data.frame(distmat)
  setDT(distdf, keep.rownames = "id")

  setDT(data, keep.rownames = "id")

  inner_join(data, distdf, by = "id")

}

gs_langdatacleaner <- function(data = NULL, rm = NULL, sel = NULL, id = NULL, structure = NULL, rmtypes = c("id", "meta", "bk", "fk")){

  structure <- suppressMessages(dplyr::left_join(data.frame("colnames" = colnames(data)), structure))
  types <- structure$type
  cbinary <- which(types == "asymm" | types == "symm")
  if(is.null(id)){id <- structure$colnames[(tolower(types) == "id")]}

  # reclass
  data[data == "#N/A" | data == "<NA>" | data == "NA" | data == "?"  | data == "" | data == " "] <- NA
  if(!is.null(cbinary)){
    bindat <- data[, cbinary]
    bindat[bindat == "Y" | bindat == "y" | bindat == 1] <- TRUE
    bindat[bindat == "N" | bindat == "n" | bindat == 0] <- FALSE
    data[, cbinary] <- bindat
    cat("Values in binary columns (symm/asymm) reclassified to TRUE/FALSE \n")
  }

  if(!purrr::is_empty(id)){

    idmissing <- nrow(data[is.na(data[,id]),] )
    if(idmissing > 0){
      data <- data[!is.na(data[,id]),]
      message(paste(idmissing, ' rows with missing ID removed'))
    }

    # Check whether ids are unique
    freqtab <- data.frame(table(data[,id]))
    colnames(freqtab)[1] <- "id"
    colnames(freqtab)[2] <- "n"

    if(any(freqtab$n > 1)){
      duplicate <- freqtab[freqtab$n > 1, ]
      message('IDs are not unique. The following ids have duplicates:')
      print(duplicate)
      message('Rownames not set because of duplicate ids. Please use the following naming convention: glottocode_dataname_001.')
    }

    if(all(freqtab$n == 1)){
      # set rownames
      data <- as.data.frame(data)
      rownames(data) <- data[,id]
    }
  }

  # select colnames to remove/select (not by index, because types argument uses index!)
  if(!is.null(rm) & is.numeric(rm)){rm <- colnames(data)[rm]}
  if(!is.null(sel) & is.numeric(sel)){sel <- colnames(data)[sel]}

  # remove columns based on types argument (by index of types!)
  if(length(rmtypes) > 0 | length(is.na(types)) > 0){
    rmcol <- which(tolower(types) %in% rmtypes)
    rmna <- which(is.na(types))
    rmcol <- c(rmcol, rmna)
    if(!is_empty(rmcol)){data <- data[, -rmcol]}
  }

  # remove columns based on sel/rm argument, done after index-based removal
  rm <- rm[rm %in% colnames(data)]
  if(purrr::is_empty(rm)){rm <- NULL}
  if(!is.null(rm) & is.null(sel)){data <- select(data, !all_of(rm))}

  sel <- sel[sel %in% colnames(data)]
  if(purrr::is_empty(sel)){sel <- NULL}
  if(is.null(rm) & !is.null(sel)){data <- dplyr::select(data, all_of(sel))}

  return(data)

}

gs_langcondist <- function(data, types = NULL, levels = NULL, weights = NULL, structure = NULL){
  #' @param data Data frame. Rows can either be languages, or sublanguages (lower-level aspects of a language, e.g. constructions).
  #' Columns contain the variables based on which distances are calculated.
  #' @param structure Data frame specifying per column in the data (optional): type, weights. Columns should be named as follows: colnames, type, weight.

  # Specify column types and levels
  if(!is.null(structure)){
    structure <- suppressMessages(dplyr::left_join(data.frame("colnames" = colnames(data)), structure))
    if(is.null(types)){types <- structure$type}
    if(is.null(levels)){levels <- structure$levels}
    if(is.null(weights)){
      if(is.null(structure$weight)){weights <- rep(1, ncol(data))}
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
  data[cbinary] <- lapply(data[cbinary], as.logical)
  data[numer] <- lapply(data[numer], as.numeric)
  data[fact] <- lapply(data[fact], as.factor)
  data[ordfact] <- mapply(FUN = as.ordfact, x = data[ordfact], levels = levels[ordfact])
  data[ordratio] <- lapply(data[ordratio], as.numeric)
  data[logratio] <- lapply(data[logratio], as.numeric)

  dist <- cluster::daisy(x = data, metric = "gower",
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
