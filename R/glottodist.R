#' Calculate distances between languages
#'
#' @param metric either "gower" or "anderberg"
#' @param glottodata glottodata or glottosubdata, either with or without structure table.
#'
#' @return object of class \code{dist}
#' @export
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottodata)
#'
#' glottosubdata <- glottoget("demosubdata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottosubdata)
#' # glottoplot(glottodist)
#'
#'
#' @section Details:
#' The `glottodist` function returns a `dist` object with respect to either Gower distance or Anderberg dissimilarity.
#' The Anderberg dissimilarity is defined as follows.
#' Consider a categorical dataset \eqn{L} containing \eqn{N} objects \eqn{X_1, \cdots, X_N} defined over a set of \eqn{d} categorical features where \eqn{A_k} denotes the \eqn{k-}th feature.
#' The feature \eqn{A_k} take \eqn{n_k} values in the given dataset which are denoted by \eqn{\mathcal{A}_k}. We regard `NA` as a new value.
#' We also use the following notations:
#'
#' \itemize{
#' \item \eqn{f_k(x)}: The number of times feature \eqn{A_k} takes the value \eqn{x} in the dataset \eqn{L}.
#' If \eqn{x\notin\mathcal{A}_k}, \eqn{f_k(x)=0}.
#' \item \eqn{\hat{p}_k(x)}: The sample frequency of feature \eqn{A_k} to take the value \eqn{x} in the dataset \eqn{L}. \eqn{\hat{p}_k(x)=\frac{f_k(x)}{N}}.
#' }
#'
#' The Anderberg dissimilarity of \eqn{X} and \eqn{Y} is defined in the form of:
#' \eqn{d(X_i, X_j)=
#' \frac{D}{D+S},
#' }
#' where \deqn{D = \sum\limits_{k\in \{1\leq k \leq d: X_k \neq Y_k\}} w_k * \delta^{(k)}_{ij}
#'  \tau_{ij}^{(k)}\left(\frac{1}{2\hat{p}_k(X_k)\hat{p}_k(Y_k)}\right)\frac{2}{n_k(n_k+1)},}
#' and
#' \deqn{S = \sum\limits_{k\in \{1\leq k \leq d: X_k = Y_k\}} w_k * \delta^{(k)}_{ij}\left(\frac{1}{\hat{p}_k(X_k)}\right)^2\frac{2}{n_k(n_k+1)}}
#'
#' The numeber \eqn{w_k} gives the weight of the \eqn{k}-th feature,
#' and the numebr \eqn{\delta^{(k)}_{ij}} is equal to either \eqn{0} or \eqn{1}.
#' It is equal to \eqn{0} when the type of the \eqn{k}-th feature is asymmetric binary and both values of \eqn{X_i} and \eqn{X_j} are \eqn{0},
#' or when either value of the \eqn{k}-th feature is missing,
#' otherwise, it is equal to \eqn{1}.
#' When \eqn{X_k \neq Y_k} and the type of \eqn{A_k} is "ordered",
#' \eqn{\tau_{ij}^{(k)}} is equal to the normalized difference of \eqn{X_k} and \eqn{Y_k},
#' otherwise \eqn{\tau_{ij}^{(k)}} is equal to \eqn{1}.
#'
#'
glottodist <- function(glottodata, metric="gower"){
  # Calaulate the dist
  params <- glottodist_cleaned(glottodata = glottodata)

  glottodata <- params$glottodata
  weights <- params$weights
  type <-  params$type
  type_code <- params$type_code

  if(metric == "gower"){
    glottodist <- cluster::daisy(x = glottodata,
                                 metric = "gower",
                                 type = type,
                                 weights = weights)
  }
  else if(metric == "anderberg"){
    glotto_types <- names(type)[
      type |>
        sapply(length) != 0]

    if (!purrr::is_empty(intersect(glotto_types, c("numeric", "ordratio", "logratio")))){
      stop("The Anderberg dissimilarity is only applicable when the type of glottodata is not
           'numeric', 'ordratio' and 'logratio'.")
    }
    else{
      glottodist <- glottodist_anderberg(glottodata = glottodata,
                                         type = type,
                                         weights = weights)
      glottodist <- add_class(object = glottodist, class = "dissimilarity")
      attr(glottodist, which="Metric") <- "anderberg"

      attr(glottodist, which="Types") <- type_code
    }
    }
  glottodist <- add_class(object = glottodist, class = "glottodist")
  glottodist
}



#' Title
#'
#' @param glottodata
#'
#'
glottodist_cleaned <- function(glottodata){
  rlang::check_installed("cluster", reason = "to use `glottodist()`")

  if(glottocheck_isglottosubdata(glottodata)){
    glottodata <- glottojoin(glottodata)
    id <- "glottosubcode"
  } else if(glottocheck_isglottodata(glottodata)){
    id <- "glottocode"
  } else {
    stop("glottodata object does not adhere to glottodata/glottosubdata format. Use glottocreate() or glottoconvert().")
    }

  if(!glottocheck_hasstructure(glottodata) ){
    stop("structure table not found. You can create one using glottocreate_structuretable() and add it with glottocreate_addtable().")
  }

  glottodata <- glottoclean(glottodata)
  structure <- glottodata[["structure"]]
  glottodata <- glottosimplify(glottodata)

  duplo <- sum(duplicated(glottodata) | duplicated(glottodata, fromLast = TRUE))
  if(duplo != 0){
    message(paste0("This glottodata contains ", duplo, " rows which have at least one duplicate, and ", nrow(glottodata) - duplo, " unique rows \n"))
    message(paste0("When plotting, you will see ",  nrow(glottodata) - sum(duplicated(glottodata)), " points (unique rows + one of each duplicate) \n"))
  }

  # structure table:
  if("varname" %nin% colnames(structure) ){
    # colnames(structure)[1] <- "varname"
    stop("The structure table does not contain a 'varname' column, please add it.")
  }

  glottodata <- tibble::column_to_rownames(glottodata, id)

   if(length(colnames(glottodata)) != length(structure$varname) ){
    message(paste("\n The number of variables in ", ifelse(id == "glottocode", "glottodata", "glottosubdata"), "differs from the number of variables in the structure table. \n") )
    nostruc <- colnames(glottodata)[colnames(glottodata) %nin% structure$varname]
    namesnostruc <- paste0(nostruc, collapse = ",")
    novar <- structure$varname[structure$varname %nin% colnames(glottodata)]
    namesnovar <- paste0(novar, collapse = ",")
    if(!purrr::is_empty(nostruc)){
      message(paste0("\n The following variables exist in the data, but are not defined in the structure table (and will be ignored): \n ", namesnostruc))
    }
    if(!purrr::is_empty(novar)){
      message(paste0("\n The following variables are defined in the structure table but do not exist in the data (and will be ignored): \n", namesnovar))
    }
  }
  structure <- suppressMessages(dplyr::left_join(data.frame("varname" = colnames(glottodata)), structure))

  message(paste0("\n ", length(structure$varname), " variables remaining \n"))


  # type
  if(!("type" %in% colnames(structure) ) ){
    stop('No type column found in structure. Please add a type column.')
  }

  dropvars <- which(structure$type %nin% glottocreate_lookuptable()$type_lookup )
  if(!purrr::is_empty(dropvars)){
    dropvarnames <- paste0(colnames(glottodata)[dropvars], collapse = ",")
    message(paste0("The following variables are ignored in distance calculation (their type is not one of the pre-specified types): \n", dropvarnames))
    message("Please make sure that all types are one of: ", paste(glottocreate_lookuptable()$type_lookup, collapse = ", "))
    glottodata <- glottodata[,-dropvars]
    structure <- structure[-dropvars, ]
    message(paste0("\n ", length(structure$varname), " variables remaining \n"))
  }

  symm <- which(structure$type == "symm")
  asymm <- which(structure$type == "asymm")
  numer <- which(structure$type == "numeric")
  fact <- which(structure$type == "factor")
  ordfact <- which(structure$type == "ordered")
  ordratio <- which(structure$type == "ordratio")
  logratio <- which(structure$type == "logratio")

  type <- list(symm = symm, asymm = asymm, ordratio = ordratio,
               logratio = logratio, numeric=numer, factor=fact,
               ordered=ordfact)

  type_names <- c("asymm", "symm", "factor", "ordered", "logratio", "ordratio", "numeric")
  type_names_simp <- c("A", "S", "N", "O", "I", "T", "I")

  type_code <- type_names_simp[match(structure$type, type_names)]

  # levels
  if(any(colnames(structure) == "levels")){
  levels <- structure$levels
  }

  cbinary <- c(symm, asymm)

  # set type
  glottodata[cbinary] <- lapply(glottodata[cbinary], as.logical)
  glottodata[numer] <- lapply(glottodata[numer], as.numeric)
  glottodata[fact] <- lapply(glottodata[fact], as.factor)
  if(!purrr::is_empty(ordfact)){
  glottodata[ordfact] <- mapply(FUN = as.ordfact, x = glottodata[ordfact], levels = levels[ordfact])
  }
  glottodata[ordratio] <- lapply(glottodata[ordratio], as.numeric)
  glottodata[logratio] <- lapply(glottodata[logratio], as.numeric)


  if(glottocheck_twolevels(glottodata)==FALSE){
    message("\n\n Because there are variables with less than two levels, the following warning messages might be generated. \n For factors: \n 'In min(x) : no non-missing arguments to min; returning Inf' \n 'In max(x) : no non-missing arguments to max; returning -Inf' \n And for symm/asymm: 'at least one binary variable has not 2 different levels' ")
    message("It is adviced to remove those variables from the data. You can do this by running glottoclean()")
    message("run glottocheck() to see which ones). \n\n")
  }

  # weights
  if(all(is.na(structure$weight))){
        weights <- rep(1, nrow(structure))
        message('All weights are NA. Default is to weight all variables equally: all weights set to 1')
  } else{
  weights <- as.numeric(structure$weight)
  if(!purrr::is_empty(weights[is.na(weights)])){
    weights[is.na(weights)]  <- 1
    message('Some weights are NA. Missing weights set to 1')
  }
  }



  return(list(glottodata=glottodata,
              weights=weights,
              type = type,
              type_code = type_code))

}



#' #' Calculate distances between languages based on constructions
#' #'
#' #' @param data data
#' #' @param index index
#' #' @param glottocodes glottocodes
#' #' @param aggregate aggregate
#' #' @param structure structure
#' #'
#' #' @noRd
#' glottocondist <- function(data = NULL, index = "constructions", glottocodes = NULL, aggregate = "mean", structure = NULL){
#'
#'
#'
#'   # Calculate distance matrices
#'   if(index == "languages"){
#'     #
#'   }
#'
#'   if(index == "threshold"){
#'     #
#'   }
#'
#'   outmatlang <- round(outmatlang, 3)
#'   return(outmatlang)
#' }
#'
#'
#'
#'
#'
#' #' Aggregate distances between constructions per language
#' #'
#' #' @param condist distance matrix based on constructions
#' #' @param glottocodes Vector of glottocodes
#' #' @param aggregation One of c('mean', 'min', 'sum') indicating how distances should be aggregated to language level.('min' returns best match)
#' #' @noRd
#' glottocondist_agg <- function(condist, glottocodes, aggregation){
#'
#'   if(inherits(condist, what = "dist" )){
#'     distmat <- as.matrix(condist)
#'   }
#'   if(is.matrix(condist)){
#'     distmat <- condist
#'   }
#'
#'   outmatlang <- glottocreate_emptydistmat(names = glottocodes)
#'
#'   for (i in seq_along(glottocodes)) {
#'     for (j in seq_along(glottocodes)) {
#'       if(glottocodes[i]!=glottocodes[j]) {
#'
#'         # Subset distance matrix for language a and b
#'         a <- stringr::str_detect(rownames(distmat), glottocodes[i])
#'         b <- stringr::str_detect(rownames(distmat), glottocodes[j])
#'         distmat_ab <- distmat[a,b, drop = F]
#'
#'         if(!all(is.na(distmat_ab))){
#'           # Average of row wise aggregated values
#'           avgrow <- mean(apply(distmat_ab, 1, FUN=aggregation, na.rm = TRUE))
#'           # TO DO: median, range, IQR, etc.
#'           # Average of col wise aggregated values
#'           avgcol <- mean(apply(distmat_ab, 2, FUN=aggregation, na.rm = TRUE))
#'           # TO DO: output avgrow and avgcol (distance from perspective of lang a and b is not the same)
#'           out_ab <- (avgrow + avgcol)/2
#'         }
#'
#'         if(all(is.na(distmat_ab))){
#'           # Only NA in both groups
#'           out_ab <- NA
#'         }
#'
#'         outmatlang[i,j] <- out_ab
#'       } else if(glottocodes[i]==glottocodes[j]) {
#'         outmatlang[glottocodes[i],glottocodes[j]] <- 0}
#'     }
#'   }
#'   message('See you later, aggregator!')
#'   return(outmatlang)
#' }
#'
#' #' Convert distances between constructions per language
#' #'
#' #' Default is to use all pairwise distances to calculate average.
#' #'
#' #' @param condist Distance matrix
#' #' @param glottocodes Vector of glottocodes
#' #' @param groups Vector of two groups (e.g. language families) for which average should be calculated.
#' #' @param thresval Threshold value
#' #' @param threstype Threshold type
#' #' @noRd
#' #'
#' glottocondist_con2lang <- function(condist, glottocodes, groups = NULL, thresval = NULL, threstype = "absolute"){
#'
#'   distmat <- as.matrix(condist)
#'
#'   if(is.null(groups) & is.null(thresval)){
#'     thr <- mean(condist)
#'     message(paste0('Mean between all constructions is: ', round(thr, 3)))
#'   }
#'   if(!is.null(groups) & is.null(thresval)){
#'     if(length(unique(groups)) >2){stop('Maximum number of groups is 2')}
#'     group1 <- glottocodes[which(groups %in% unique(groups)[1])]
#'     group2 <- glottocodes[which(groups %in% unique(groups)[2])]
#'
#'     groups <- sub("\\_.*", "", names(condist))
#'     a <- groups %in% group1
#'     b <- groups %in% group2
#'     distmat_ab <- distmat[a,b, drop = F]
#'     thr <- mean(distmat_ab)
#'     message(paste0('Mean between groups is: ', round(thr, 3)))
#'   }
#'   if(!is.null(thresval) & is.null(groups)){
#'     thr <- thresval
#'   }
#'   if(!is.null(thresval) & !is.null(groups)){
#'     message('Provide either thresval or groups, not both')
#'   }
#'
#'   if(threstype == "absolute"){
#'     message('\n +1 = more dissimilar than average, \n -1 less dissimilar than average')
#'     distmat <- ifelse(distmat > thr, 1, -1)
#'     return(distmat)
#'   }
#'
#'   if(threstype == "center"){
#'     message('\n Positive values = more dissimilar than average, \n Negative values = less dissimilar than average')
#'     distmat <- distmat - thr # equivalent: scale(x = distmat, center = rep(thr, ncol(distmat)), scale = F)
#'     return(distmat)
#'   }
#'
#' }
