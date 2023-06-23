#' Create a cleaned glottodata fitting for calculating dissimilarities
#'
#' @param glottodata glottodata or glottosubdata, either with or without structure table.
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodata_cleaned(glottodata)
#'
#' glottosubdata <- glottoget("demosubdata", meta = TRUE)
#' glottodata_cleaned(glottosubdata)
glottodata_cleaned <- function(glottodata){
#  rlang::check_installed("cluster", reason = "to use `glottodist()`")

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

  # glottodist <- cluster::daisy(x = glottodata, metric = "gower",
  #                             type = list(symm = symm, asymm = asymm, ordratio = ordratio, logratio = logratio),
  #                             weights = weights)
  # glottodist <- add_class(object = glottodist, class = "glottodist")
  glottodata
}


#' Calculate Anderberg dissimilarity between two objects
#'
#' @param glottodata A dataframe
#' @param i An integer
#' @param j An integer
#'
#' @return A numeric value
#' @export
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodata <- glottodata_cleaned(glottodata)
#' anderberg_dissimilarity(glottodata, 1, 3)
#'
#' glottosubdata <- glottoget("demosubdata", meta = TRUE)
#' glottosubdata <- glottodata_cleaned(glottosubdata)
#' anderberg_dissimilarity(glottosubdata, 1, 3)
anderberg_dissimilarity <- function(glottodata, i, j) {
  # glottodata <- glottodata_cleaned(glottodata)
  glottodata_freq_list <- glottodata |>
    plyr::alply(
      2, table
    ) |>
    lapply (
      FUN = function(x){
        x / sum(x)
      }
    ) # glottodata_freq_list is a list containing the frequencies of each value in each feature.

  glottodata_val_counts <- glottodata_freq_list |>
    sapply(length) # glottodata_val_counts is a vector containing the amount of different values for each features.

  feature_same_idx <- which(glottodata[i, ] == glottodata[j, ])
  feature_diff_idx <- which(glottodata[i, ] != glottodata[j, ])

  val_same <- glottodata[i, feature_same_idx] # val_same contains all the same values of object i and object j
  freq_same <- mapply(
    FUN = function(x, y){
      x[as.character(y)]
    },
    glottodata_freq_list[feature_same_idx], val_same
  )

  if (length(feature_same_idx) == 0){
    part_same <- 0
  } else {
    part_same <- mapply(
      FUN = function(p, n){
        (1/p)^2 * 2 / (n * (n+1))
      },
      freq_same, glottodata_val_counts[feature_same_idx]
    ) |>
      sum()
  }

  if (length(feature_diff_idx) == 0){
    part_diff <- 0
  } else {
    part_diff <- feature_diff_idx |>
      sapply(
        FUN = function(idx){
          1 / (2 * glottodata_freq_list[[idx]][as.character(glottodata[i, idx])] * glottodata_freq_list[[idx]][as.character(glottodata[j, idx])]) *
            2 / (glottodata_val_counts[idx] * (glottodata_val_counts[idx] + 1))
        }
      ) |>
      sum()
  }

  if ((part_same + part_diff) != 0){
    anderberg.dissimilar <- part_diff / (part_same + part_diff)
  }
  else {
    anderberg.dissimilar <- 1
  }

  return(anderberg.dissimilar)

}

#' Anderberg dissimilarity
#'
#' `glottodist_anderberg()` calaulate the Anderberg dissimilarity
#'
#' @section Details:
#'
#' Consider a categorical dataset \eqn{L} containing \eqn{N} objects defined over a set of \eqn{d} categorical features where \eqn{A_k} denotes the \eqn{k-}th feature.
#' The feature \eqn{A_k} take \eqn{n_k} values in the given dataset which are denoted by \eqn{\mathcal{A}_k}.
#' We also use the following notation:
#'
#' \itemize{
#' \item \eqn{f_k(x)}: The number of times feature \eqn{A_k} takes the value \eqn{x} in the dataset \eqn{L}.
#' If \eqn{x\notin\mathcal{A}_k}, \eqn{f_k(x)=0}.
#' \item \eqn{\hat{p}_k(x)}: The sample frequency of feature $A_k$ to take the value $x$ in the dataset \eqn{L}. \eqn{\hat{p}_k(x)=\frac{f_k(x)}{N}}.
#' }
#'
#'The Anderberg dissimilarity of \eqn{X} and \eqn{Y} is defined as:
#'\deqn{D(X, Y)=
#'\frac{\sum\limits_{k\in \{1\leq k \leq d: X_k \neq Y_k\}}\left(\frac{1}{2\hat{p}_k(X_k)\hat{p}_k(Y_k)}\right)\frac{2}{n_k(n_k+1)}}
#'{\sum\limits_{k\in \{1\leq k \leq d: X_k=Y_k\}}\left(\frac{1}{\hat{p}_k(X_k)}\right)^2\frac{2}{n_k(n_k+1)} +
#'\sum\limits_{k\in \{1\leq k \leq d: X_k \neq Y_k\}}\left(\frac{1}{2\hat{p}_k(X_k)\hat{p}_k(Y_k)}\right)\frac{2}{n_k(n_k+1)}}.}
#'
#'
#'
#' @param glottodata glottodata or glottosubdata, either with or without structure table.
#'
#' @return object of class \code{dist}
#' @export
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist_anderberg(glottodata)
#'
#' glottosubdata <- glottoget("demosubdata", meta = TRUE)
#' glottodist_anderberg(glottosubdata)
glottodist_anderberg <- function(glottodata){
  glottodata <- glottodata_cleaned(glottodata)
  n <- nrow(glottodata)
  dist_matrix <- matrix(nrow=n, ncol=n)
  for (i in 1:(n-1)){
    for(j in (i + 1):n){
      dist_matrix[i, j] <- anderberg_dissimilarity(glottodata, i, j)
      dist_matrix[j, i] <- dist_matrix[i, j]
    }
  }

  for (i in 1:n){
    dist_matrix[i, i] <- 0
  }
  return(as.dist(dist_matrix))
}






