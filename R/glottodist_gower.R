#' Title
#'
#' @param a a vector
#' @param b a vector
#' @param weights a vector
#'
#' @return a numeric number
#'
#' @noRd
gower.a.b <- function(a, b, weights){
  non_na_idx <- intersect(which(!is.na(a)), which(!is.na(b)))
  a <- a[non_na_idx]
  b <- b[non_na_idx]
  weights <- weights[non_na_idx]
  sum(as.numeric(a != b) * weights) / length(a)
}

#' Title
#'
#' @param a a vector
#' @param B a matrix
#' @param weights a vector
#'
#' @return a numeric number
#'
#' @noRd
gower.a.B <- function(a, B, weights) {
  B |>
    apply(MARGIN = 1, FUN = function(b){
      return(gower.a.b(a=a, b=b, weights=weights))
    }) |>
    min()
}

#' Title
#'
#' @param A a matrix
#' @param B a matrix
#' @param weights a vector
#'
#' @return a numeric number
#'
#' @noRd
gower.A.B <- function (A, B, weights) {
  A |>
    apply(MARGIN = 1, FUN = function(a){gower.a.B(a, B, weights)}) |>
    mean()
}

#' A function to compute the average of pairwise gower distances with weight weights.
#'
#' @param glottodata a dataframe
#' @param idx_A a vector
#' @param idx_B a vector
#' @param weights a vector
#'
#' @return a numeric number
#'
#' @noRd
avg_gower <- function(glottodata, idx_A, idx_B, weights){
  idx_A |>
    sapply(
      FUN = function(idx_1){
        idx_B |>
          sapply(
            FUN = function(idx_2){
              gower.a.b(a = glottodata[idx_1, ], b = glottodata[idx_2, ], weights = weights)
            }) |>
          mean()
      }) |>
    mean()
}

#' Compute the MC distance between two point cloud A and B
#'
#' @param glottodata a dataframe
#' @param idx_A a vector
#' @param idx_B a vector
#' @param weights a vector
#'
#' @return a numeric number
#'
#' @noRd
gower.MC <- function(glottodata, idx_A, idx_B, weights){
  (gower.A.B(A = glottodata[idx_A, ], B = glottodata[idx_B, ], weights = weights) +
     gower.A.B(A = glottodata[idx_B, ], B = glottodata[idx_A, ], weights = weights)) / 2
}

#' Title
#'
#' @param glottosubdata a glottosubdata
#'
#' @return a numeric number
#'
#' @noRd
glottodist_gower_MC <- function(glottosubdata){
  glottosubdata_splfy <- glottosimplify(glottosubdata, submerge = F)
  glottocodes <- glottocode_get(glottosubdata_splfy)
  cnstrn_count <- from_to_idx(glottosubdata_splfy |>
                                sapply(nrow))

  params <- glottodist_cleaned(glottodata = glottosubdata, one_level_drop = F) |>
    suppressMessages()
  glottodata <- params$glottodata
  weights <- params$weights
  type <-  params$type
  type_code <- params$type_code

  dim <- length(glottosubdata_splfy)
  dist_matrix <- matrix(nrow=dim, ncol=dim)

  for (i in 1:(dim-1)){
    for(j in (i + 1):dim){
      dist_matrix[i, j] <- gower.MC(glottodata=glottodata, idx_A = cnstrn_count[[i]],
                                    idx_B = cnstrn_count[[j]], weights=weights)
      dist_matrix[j, i] <- dist_matrix[i, j]
    }
  }

  for (i in 1:dim){
    dist_matrix[i, i] <- 0
  }
  colnames(dist_matrix) <- glottocodes
  rownames(dist_matrix) <- glottocodes
  return(stats::as.dist(dist_matrix))
}

#' Title
#'
#' @param lg a dataframe
#' @param a_idx a vector
#' @param f_idx a vector
#'
#' @return a numeric number
#'
#' @noRd
lg_fixed_avg_count <- function(lg, a_idx, f_idx) {
  # A function to count how many constructions having both m_idx and f_idx being "Y" or TRUE in language lg.
  # lg is a dataframe, m_idx is an index number, f_idx is an index number.
  avg_indices <- which(lg[, a_idx] == "Y" | lg[, a_idx] == TRUE)
  fixed_indices <- which(lg[, f_idx] == "Y" | lg[, f_idx] == TRUE)
  intersect(avg_indices, fixed_indices) |>
    length()
}

#' Title
#'
#' @param lg1 a dataframe
#' @param lg2 a dataframe
#' @param a_idx a vector
#' @param f_idx a vector
#'
#' @return a numeric number
#'
#' @noRd
SIM <- function(lg1, lg2, a_idx, f_idx){
  lg_cnt_1 <- lg_fixed_avg_count(lg = lg1, a_idx = a_idx, f_idx = f_idx)
  lg_cnt_2 <- lg_fixed_avg_count(lg = lg2, a_idx = a_idx, f_idx = f_idx)

  if (lg_cnt_1 == 0 && lg_cnt_2 == 0){
    result <- 1
  } else {
    result <- min(lg_cnt_1 / lg_cnt_2, lg_cnt_2 / lg_cnt_1)
  }
  return(result)
}

#' Title
#'
#' @param lg1 a dataframe
#' @param lg2 a dataframe
#' @param fixed_idx a vector
#' @param avg_idx a vector
#'
#' @return a numeric number
#'
#' @noRd
FMI <- function(lg1, lg2, fixed_idx, avg_idx) {
  fixed_idx |>
    sapply(FUN = function(f_idx){
      avg_idx |>
        sapply(
          FUN = function(a_idx){
            1 - SIM(lg1 = lg1, lg2 = lg2, a_idx = a_idx, f_idx = f_idx)
          }
        ) |>
        mean()
    }) |>
    mean()
}

#' A function to compute the distance matrix w.r.t. FMI
#'
#' @param glottosubdata a glottosubdata
#' @param avg_idx a vector
#' @param fixed_idx a vector
#'
#' @return a numeric number
#'
#' @noRd
glottodist_FMI <- function(glottosubdata, avg_idx, fixed_idx){
  glottosubdata_splfy <- glottosimplify(glottosubdata, submerge = F)
  glottocodes <- glottocode_get(glottosubdata_splfy)
  cnstrn_count <- from_to_idx(glottosubdata_splfy |>
                                sapply(nrow))

  params <- glottodist_cleaned(glottodata = glottosubdata, one_level_drop = F) |>
    suppressMessages()
  glottodata <- params$glottodata
  weights <- params$weights
  type <-  params$type
  type_code <- params$type_code

  dim <- length(glottosubdata_splfy)
  dist_matrix <- matrix(nrow=dim, ncol=dim)

  for (i in 1:(dim-1)){
    for(j in (i + 1):dim){
      dist_matrix[i, j] <- FMI(lg1 = glottodata[cnstrn_count[[i]], ],
                               lg2 = glottodata[cnstrn_count[[j]], ],
                               avg_idx = avg_idx, fixed_idx = fixed_idx)
      dist_matrix[j, i] <- dist_matrix[i, j]
    }
  }

  for (i in 1:dim){
    dist_matrix[i, i] <- 0
  }

  colnames(dist_matrix) <- glottocodes
  rownames(dist_matrix) <- glottocodes
  return(stats::as.dist(dist_matrix))
}


#' Title
#'
#' @param glottodata a dataframe
#' @param idx_A a vector
#' @param idx_B a vector
#' @param avg_idx a vector
#' @param fixed_idx a vector
#' @param weights a vector
#'
#' @return a numeric number
#'
#' @noRd
gower_Indexing <- function(glottodata, idx_A, idx_B, avg_idx, fixed_idx, weights){
  # glottodata is a dataframe
  sum <- 0

  for (s in avg_idx){
    cnstn_1 <- idx_A[which((glottodata[idx_A, s] == "Y") | (glottodata[idx_A, s] == TRUE))]
    cnstn_2 <- idx_B[which((glottodata[idx_B, s] == "Y") | (glottodata[idx_B, s] == TRUE))]

    if (!identical(cnstn_1, integer(0)) && !identical(cnstn_2, integer(0))) {
      avg <- avg_gower(glottodata = glottodata[, fixed_idx],
                       idx_A = cnstn_1, idx_B = cnstn_2,
                       weights = weights[fixed_idx])
      sum <- sum + avg
    }
  }
  return(sum / length(avg_idx))
}

#' Title
#'
#' @param glottosubdata a glottosubdata
#' @param avg_idx a vector
#' @param fixed_idx a vector
#'
#' @return a numeric number
#'
#' @noRd
glottodist_gower_Indexing <- function(glottosubdata, avg_idx, fixed_idx){
  glottosubdata_splfy <- glottosimplify(glottosubdata, submerge = F)
  glottocodes <- glottocode_get(glottosubdata_splfy)
  cnstrn_count <- from_to_idx(glottosubdata_splfy |>
                                sapply(nrow))

  params <- glottodist_cleaned(glottodata = glottosubdata, one_level_drop = F) |>
    suppressMessages()
  glottodata <- params$glottodata
  weights <- params$weights
  type <-  params$type
  type_code <- params$type_code

  dim <- length(glottosubdata_splfy)
  dist_matrix <- matrix(nrow=dim, ncol=dim)

  for (i in 1:(dim-1)){
    for(j in (i + 1):dim){
      dist_matrix[i, j] <- gower_Indexing(glottodata=glottodata,
                                    idx_A = cnstrn_count[[i]], idx_B = cnstrn_count[[j]],
                                    avg_idx = avg_idx, fixed_idx = fixed_idx,
                                    weights = weights)
      dist_matrix[j, i] <- dist_matrix[i, j]
    }
  }

  for (i in 1:dim){
    dist_matrix[i, i] <- 0
  }

  colnames(dist_matrix) <- glottocodes
  rownames(dist_matrix) <- glottocodes
  return(stats::as.dist(dist_matrix))
}














