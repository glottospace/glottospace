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
#' glottodist <- glottodist(glottodata = glottodata, metric="anderberg")
#'
#' glottosubdata <- glottoget("demosubdata", meta = TRUE)
#' glottodist <- glottodist(glottodata = glottosubdata)
#'
#'
#' @section Details:
#' The function ``glottodist'' returns a ``dist'' object with respect to either Gower distance or Anderberg dissimilarity.
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
#' where \deqn{D = \sum\limits_{k\in \{1\leq k \leq d: X_k \neq Y_k\}} w_k * \delta^{(k)}_{ij} *
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
#' @references
#' Andergerg M.R. (1973). Cluster analysis for applications. Academic Press, New York.
#'  \cr
#'  \cr
#' Boriah S., Chandola V., Kumar V. (2008). Similarity measures for categorical data: A comparative evaluation.
#' In: Proceedings of the 8th SIAM International Conference on Data Mining, SIAM, p. 243-254.
#'
#'
glottodist <- function(glottodata, metric="gower"){
  # Calaulate the dist
  params <- glottodist_cleaned(glottodata = glottodata) |>
    suppressMessages()

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
           \"numeric\", \"ordratio\" and \"logratio\".")
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
#' @noRd
#'
glottodist_cleaned <- function(glottodata, ...){
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

  glottodata <- glottoclean(glottodata, ...)
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

#' Calculate construction-based distances between languages
#'
#' @param glottosubdata an glottosubdata object
#' @param metric either "gower" or "anderberg"
#' @param index_type either "mci" or "ri" or "fmi"
#' @param avg_idx the feature indices over which the average of distances is computed, it must be given when index_type is either "ri" or "fmi".
#' @param fixed_idx the feature indices over which the distance of two constructions is computed, it must be given when index_type is either "ri" or "fmi".
#'
#' @return object of class \code{dist}
#'
#' @export
#'
#' @examples
#' glottosubdata_cnstn <- glottoget(glottodata = "demosubdata_cnstn")
#' glottodist_subdata(glottosubdata = glottosubdata_cnstn, metric = "gower", index_type = "mci")
#' glottodist_subdata(glottosubdata = glottosubdata_cnstn, metric = "gower", index_type = "ri",
#'                    avg_idx = 1:4, fixed_idx = 5:7)
#' glottodist_subdata(glottosubdata = glottosubdata_cnstn, index_type = "fmi",
#'                    avg_idx = 1:4, fixed_idx = 5:7)
#'
#' @section Details:
#' The function ``glottodist_subdata'' returns a ``dist'' object,
#' the input is a glottosubdata object,
#' it computes the construction-based distance between languages,
#' we refer to the observations of each language as constructions.
#' The distance \eqn{d(A_i, B_j)} between two constructions \eqn{A_i} in a language \eqn{A} and \eqn{B_j} in a language \eqn{B}
#' is determined by the argument ``metric'',
#' whose value is either ``gower'' or ``anderberg''.
#' When ``index_type'' is ``mci'',
#' it returns the ``matching constructions index'':
#'
#' \eqn{MCI(A, B) := \frac{1}{2|A|}\sum\limits_{A_i\in A}\min\limits_{B_j\in B}d(A_i, B_j) +
#' \frac{1}{2|B|}\sum\limits_{B_i\in B}\min\limits_{A_j\in A}d(A_j, B_i)}.
#' When ``index_type'' is ``ri'',
#' it returns the ``relative index'':
#'
#' \eqn{RI(A, B) = \frac{1}{|M|}\sum\limits_{s\in M}\textrm{AVG}_{A_i(s) = 1 \textrm{ and } B_j(s) = 1}d(A_i^F, B_j^F)},
#' here \eqn{M} is the indices of a subset of variables given by the argument ``avg_idx'' and \eqn{F} is the indices of a subset of variables given by the argument ``fixed_idx'',
#' the restricted constructions \eqn{A_i^F} and \eqn{B_j^F} are defined as the constructions \eqn{A_i}, \eqn{B_j} restricted to ``fixed_idx'' \eqn{F}.
#' When ``index_type'' is ``fmi'',
#' it returns the ``form-meaning index'':
#'
#' \eqn{FMI(A, B) = \frac{1}{|M||F|} \sum\limits_{s\in M, p\in F} \Big(1 - SIM(\{(A_i^M(s)=1 \textrm{ and }A_i^F(p)=1)\},
#' \{B_j^M(s) = 1 \textrm{ and }B_j^F(p) = 1\})\Big)},
#' here \eqn{SIM(X, Y)=\min(|X|/|Y|, |Y|/|X|)}, if both \eqn{X} and \eqn{Y} are empty,
#' \eqn{SIM(X, Y)=1}.
#'
#'
glottodist_subdata <- function(glottosubdata, metric = NULL, index_type = NULL, avg_idx=NULL, fixed_idx=NULL){
  if (tolower(index_type) %in% c("ri", "fmi") &&
      (is.null(avg_idx) || is.null(fixed_idx))){
    stop("Both the arguments avg_idx and fixed_idx should be provided.")
  }
  metric <- tolower(metric)
  index_type <- tolower(index_type)

  if (index_type == "fmi"){
    glottodata_dist <- glottodist_FMI(glottosubdata = glottosubdata, avg_idx = avg_idx, fixed_idx = fixed_idx)
  } else if (metric == "gower" && index_type == "mci"){
    glottodata_dist <- glottodist_gower_MC(glottosubdata = glottosubdata)
  } else if (metric == "gower" && index_type == "ri"){
    glottodata_dist <- glottodist_gower_Indexing(glottosubdata = glottosubdata,
                                                 avg_idx = avg_idx, fixed_idx = fixed_idx)
  } else if (metric == "anderberg" && index_type == "mci"){
    glottodata_dist <- glottodist_anderberg_MC(glottosubdata = glottosubdata)
  } else if (metric == "anderberg" && index_type == "ri"){
    glottodata_dist <- glottodist_anderberg_Indexing(glottosubdata = glottosubdata,
                                                     avg_idx = avg_idx, fixed_idx = fixed_idx)
  }
  return(glottodata_dist)
}
