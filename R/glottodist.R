#' Calculate distances between languages
#'
#' @param glottodata glottodata or glottosubdata, either with or without structure table.
#' @param structure If glottodata is a table without a structure table, you can add it separately. To create a structure table, you should run glottocreate_structuretable() and you can add it with glottocreate_addtable()
#' @param id By default, glottodist looks for a column named 'glottocode', if the id is in a different column, this should be specified.
#' @return object of class \code{dist}
#' @export
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
glottodist <- function(glottodata, structure = NULL, id = NULL){

  if(glottocheck_isglottosubdata(glottodata)){
    glottodata <- glottojoin(glottodata)
  } else if(!glottocheck_isglottodata(glottodata)){
    message("glottodata object does not adhere to glottodata/glottosubdata format. Use glottocreate() or glottoconvert().")
    }

  if(glottocheck_hasmeta(glottodata) & is.null(structure)){
    splitted <- glottosplitmergemeta(glottodata)
    glottodata <- splitted[[1]]
    structure <- splitted[[2]][["structure"]]
  } else if(glottocheck_hasmeta(glottodata) & !is.null(structure)){
    glottodata <- glottodata[["glottodata"]]
  } else if(!glottocheck_hasmeta(glottodata) & is.null(structure)){
    stop("structure table not found, please add one to glottodata or provide it separately.")
  }

  # glottodata:
  if(is.null(id)){
    if("glottocode" %in% colnames(glottodata) & "glottosubcode" %nin% colnames(glottodata)){
      id <- "glottocode"
      message("glottocode used as id")
    } else if("glottocode" %nin% colnames(glottodata) & "glottosubcode" %in% colnames(glottodata)){
      id <- "glottosubcode"
      message("glottosubcode used as id")
    } else if(all(c("glottocode", "glottosubcode") %in% colnames(glottodata)) ){
      id <- "glottocode"
      message("Data contains glottocodes AND glottosubcodes, glottocode used as id. If this is not what you want, please specify id.")
    } else if(all(c("glottocode", "glottosubcode") %nin% colnames(glottodata)) ){
      stop("Please provide an id, or add a 'glottocode' or 'glottosubcode' column to your data")
    }
    }
  glottodata <- tibble::column_to_rownames(glottodata, id)

  # structure table:
  if(!("varname" %in% colnames(structure) ) ){
    colnames(structure)[1] <- "varname"
    message("The structure table does not contain a 'varname' column, trying with the first column instead.")
  }

  if(length(colnames(glottodata)) != length(structure$varname) ){
    message(paste("The number of variables in ", ifelse(id == "glottocode", "glottodata", "glottosubdata"), "differs from the number of variables in the structure table") )
  }
  structure <- suppressMessages(dplyr::left_join(data.frame("varname" = colnames(glottodata)), structure))

  # type
  if(!("type" %in% colnames(structure) ) ){
    stop('No type column found in structure. Please add a type column.')
  }

  dropvars <- which(structure$type %nin% glottocreate_lookuptable()$type_lookup )
  if(!purrr::is_empty(dropvars)){
    dropvarnames <- paste0(colnames(glottodata)[dropvars], collapse = ",")
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
  if(any(colnames(structure) == "levels")){
  levels <- structure$levels
  }

  # binary columns
  cbinary <- c(symm, asymm)
  if(length(cbinary) != 0){
  allevmat <- sapply(lapply(glottodata[cbinary], as.factor), levels)
  allevuniq <- unique(c(allevmat))
  if(any(allevuniq %nin% c("T", "TRUE", "True", "true", "F", "FALSE", "False", "false"))){
    message("For some variables of type 'symm' and 'asymm', it is not clear whether they are TRUE of FALSE. It is highly recommended to run glottoclean() before running glottodist(). Attempting to convert the following values to TRUE/FALSE...")
    printmessage(allevuniq)
    glottodata[cbinary] <- glottoclean(glottodata[cbinary], structure = structure)
  }
  }
  # set type
  glottodata[cbinary] <- lapply(glottodata[cbinary], as.logical)
  glottodata[numer] <- lapply(glottodata[numer], as.numeric)
  glottodata[fact] <- lapply(glottodata[fact], as.factor)
  if(!purrr::is_empty(ordfact)){
  glottodata[ordfact] <- mapply(FUN = as.ordfact, x = glottodata[ordfact], levels = levels[ordfact])
  }
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



#' Calculate distances between languages based on constructions
#'
#' @param data data
#' @param index index
#' @param glottocodes glottocodes
#' @param aggregate aggregate
#' @param structure structure
#'
#' @noRd
glottocondist <- function(data = NULL, index = "constructions", glottocodes = NULL, aggregate = "mean", structure = NULL){



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





#' Aggregate distances between constructions per language
#'
#' @param condist distance matrix based on constructions
#' @param glottocodes Vector of glottocodes
#' @param aggregation One of c('mean', 'min', 'sum') indicating how distances should be aggregated to language level.('min' returns best match)
#' @noRd
glottocondist_agg <- function(condist, glottocodes, aggregation){

  if(any(class(condist) == "dist")){
    distmat <- as.matrix(condist)
  }
  if(is.matrix(condist)){
    distmat <- condist
  }

  outmatlang <- glottocreate_emptydistmat(names = glottocodes)

  for (i in seq_along(glottocodes)) {
    for (j in seq_along(glottocodes)) {
      if(glottocodes[i]!=glottocodes[j]) {

        # Subset distance matrix for language a and b
        a <- stringr::str_detect(rownames(distmat), glottocodes[i])
        b <- stringr::str_detect(rownames(distmat), glottocodes[j])
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
  message('See you later, aggregator!')
  return(outmatlang)
}

#' Convert distances between constructions per language
#'
#' Default is to use all pairwise distances to calculate average.
#'
#' @param condist Distance matrix
#' @param glottocodes Vector of glottocodes
#' @param groups Vector of two groups (e.g. language families) for which average should be calculated.
#' @param thresval Threshold value
#' @param threstype Threshold type
#' @noRd
#'
glottocondist_con2lang <- function(condist, glottocodes, groups = NULL, thresval = NULL, threstype = "absolute"){

  distmat <- as.matrix(condist)

  if(is.null(groups) & is.null(thresval)){
    thr <- mean(condist)
    message(paste0('Mean between all constructions is: ', round(thr, 3)))
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
    message(paste0('Mean between groups is: ', round(thr, 3)))
  }
  if(!is.null(thresval) & is.null(groups)){
    thr <- thresval
  }
  if(!is.null(thresval) & !is.null(groups)){
    message('Provide either thresval or groups, not both')
  }

  if(threstype == "absolute"){
    message('\n +1 = more dissimilar than average, \n -1 less dissimilar than average')
    distmat <- ifelse(distmat > thr, 1, -1)
    return(distmat)
  }

  if(threstype == "center"){
    message('\n Positive values = more dissimilar than average, \n Negative values = less dissimilar than average')
    distmat <- distmat - thr # equivalent: scale(x = distmat, center = rep(thr, ncol(distmat)), scale = F)
    return(distmat)
  }

}
