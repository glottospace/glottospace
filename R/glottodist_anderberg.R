#' Calculate Anderberg dissimilarity between two objects
#'
#' @param glottodata A dataframe
#' @param i An integer
#' @param j An integer
#'
#' @return A numeric value
#'
#'
#' @noRd
#'
anderberg_dissimilarity <- function(glottodata, glottodata_freq_list = NULL, glottodata_val_counts = NULL,
                                    i, j, type, weights) {

  # glotto_colnames <- colnames(glottodata)
  # if (any(glotto_colnames %in% c("glottocode", "glottocodes", "glottosubcode", "glottosubcodes"))){
  #   glotto_vals_idx <- which(tolower(glotto_colnames) %nin% c("glottocode", "glottocodes", "glottosubcode", "glottosubcodes"))
  #   glottodata <- glottodata[, glotto_vals_idx]
  # }
  #
  if (is.null(glottodata_freq_list)){
    glottodata_freq_list <- 1:ncol(glottodata) |>
      lapply(
        FUN = function(idx){
          tbl <- glottodata[, idx] |>
            table(useNA = "ifany")
          tbl / nrow(glottodata)
        })
  }

  if (is.null(glottodata_val_counts)){
    glottodata_val_counts <- glottodata_freq_list |>
      sapply(length)
  }

  delta <- rep(1, length(weights)) # Define the delta vector
  if (length(type$asymm) != 0){
    delta_0_idx <- intersect(which(glottodata[i, type$asymm] == FALSE), which(glottodata[j, type$asymm] == FALSE))
    delta[delta_0_idx] <- 0
  } # If both values of an asymm type feature are FALSE, delta is set to be 0

  feature_same_idx_na <- union(which(is.na(glottodata[i, ])), which(is.na(glottodata[j, ]))) # The indices of features with both values being NAs
  if (length(feature_same_idx_na) != 0){
    delta[feature_same_idx_na] <- 0
  } # If both values of a feature are NAs, delta is set to be 0

  tau <- rep(1, length(weights))

  if (length(type$ordered) != 0) {
    order_idx <- type$ordered

    tau[order_idx] <- order_idx |>
      sapply (
        FUN = function(x){
          if (!is.na(glottodata[i, x]) && !is.na(glottodata[j, x]) && (glottodata[i, x] != glottodata[j, x])) {
            lvl <- levels(glottodata[, x])
            result <- abs(which(lvl == glottodata[i, x]) - which(lvl == glottodata[j, x])) / (length(lvl) - 1)
          } else {
            result <- 1
          }
          return(result)
        }
      )
  }

  weights <- weights * delta * tau # Upgrade the weight

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
      FUN = function(w, p, n){
        w * (1/p)^2 * 2 / (n * (n+1))
      },
      weights[feature_same_idx], freq_same, glottodata_val_counts[feature_same_idx]
    ) |>
      sum()
  }

  if (length(feature_diff_idx) == 0){
    part_diff <- 0
  } else {
    part_diff <- feature_diff_idx |>
      sapply(
        FUN = function(idx){
          weights[idx] * 1 / (2 * glottodata_freq_list[[idx]][as.character(glottodata[i, idx])] *
                                glottodata_freq_list[[idx]][as.character(glottodata[j, idx])]) *
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


#' @param glottodata A dataframe
#' @param type A list
#' @param weights A vector
#'
#' @return object of class \code{dist}
#'
#' @noRd
#'
glottodist_anderberg <- function(glottodata, type, weights, glottodata_freq_list = NULL, glottodata_val_counts = NULL){
  n <- nrow(glottodata)
  dist_matrix <- matrix(nrow=n, ncol=n)

  if (is.null(glottodata_freq_list)){
    glottodata_freq_list <- 1:ncol(glottodata) |>
      lapply(
        FUN = function(idx){
          tbl <- glottodata[, idx] |>
            table(useNA = "ifany")
          tbl / nrow(glottodata)
        })
  } # glottodata_freq_list is a list containing the frequencies of each value in each feature.

  if (is.null(glottodata_val_counts)) {
  glottodata_val_counts <- glottodata_freq_list |>
    sapply(length) # glottodata_val_counts is a vector containing the amount of different values for each features.
  }

  for (i in 1:(n-1)){
    for(j in (i + 1):n){
      dist_matrix[i, j] <- anderberg_dissimilarity(glottodata=glottodata, glottodata_freq_list=glottodata_freq_list,
                                                   glottodata_val_counts=glottodata_val_counts,
                                                   i = i, j = j, type=type, weights=weights)
      dist_matrix[j, i] <- dist_matrix[i, j]
    }
  }

  for (i in 1:n){
    dist_matrix[i, i] <- 0
  }
  colnames(dist_matrix) <- rownames(glottodata)
  rownames(dist_matrix) <- rownames(glottodata)
  return(stats::as.dist(dist_matrix))
}

#' Title
#'
#' @param glottodata a dataframe
#' @param glottodata_freq_list a list
#' @param glottodata_val_counts a vector of counting numbers
#' @param idx_a a index number
#' @param idx_B a vector of indices
#' @param type a vector of types given by structure of glottodata
#' @param weights a vector of weights given by structure of glottodata
#'
#' @return a numeric number
#'
#' @noRd
anderberg_dissimilarity.a.B <- function(glottodata, glottodata_freq_list = NULL, glottodata_val_counts = NULL,
                                        idx_a, idx_B, type, weights){
  if (is.null(glottodata_freq_list)){
    glottodata_freq_list <- 1:ncol(glottodata) |>
      lapply(
        FUN = function(idx){
          tbl <- glottodata[, idx] |>
            table(useNA = "ifany")
          tbl / nrow(glottodata)
        })
  }
  if (is.null(glottodata_val_counts)){
    glottodata_val_counts <- glottodata_freq_list |>
      sapply(length)
  }

  idx_B |>
    sapply(FUN = function(idx_b){
      anderberg_dissimilarity(glottodata = glottodata, glottodata_freq_list = glottodata_freq_list, glottodata_val_counts = glottodata_val_counts, i = idx_a, j = idx_b, type = type, weights = weights)
    }) |>
    min()
}

#' Title
#'
#' @param glottodata a dataframe
#' @param glottodata_freq_list a list
#' @param glottodata_val_counts a vector of counting numbers
#' @param idx_A a vector of indices
#' @param idx_B a vector of indices
#' @param type a vector of types given by structure of glottodata
#' @param weights a vector of weights given by structure of glottodata
#'
#' @return a numeric number
#' @noRd
anderberg_dissimilarity.A.B <- function(glottodata, glottodata_freq_list = NULL, glottodata_val_counts = NULL,
                                        idx_A, idx_B, type, weights){
  if (is.null(glottodata_freq_list)){
    glottodata_freq_list <- 1:ncol(glottodata) |>
      lapply(
        FUN = function(idx){
          tbl <- glottodata[, idx] |>
            table(useNA = "ifany")
          tbl / nrow(glottodata)
        })
  }
  if (is.null(glottodata_val_counts)){
    glottodata_val_counts <- glottodata_freq_list |>
      sapply(length)
  }

  anderberg <- idx_A |>
    sapply(FUN = function(idx_a){
      anderberg_dissimilarity.a.B(glottodata=glottodata, glottodata_freq_list = glottodata_freq_list, glottodata_val_counts = glottodata_val_counts,
                                  idx_a = idx_a, idx_B = idx_B, type = type, weights = weights)
    }) |>
    mean()
  return(anderberg)
}

#' Title
#'
#' @param glottodata a dataframe
#' @param glottodata_freq_list a list
#' @param glottodata_val_counts a vector of counting numbers
#' @param idx_A a vector of indices
#' @param idx_B a vector of indices
#' @param type a vector of types given by structure of glottodata
#' @param weights a vector of weights given by structure of glottodata
#'
#' @return a numeric number
#' @noRd
anderberg_dissimilarity.MC <- function(glottodata, glottodata_freq_list = NULL, glottodata_val_counts = NULL,
                                       idx_A, idx_B, type, weights){
  if (is.null(glottodata_freq_list)){
    glottodata_freq_list <- 1:ncol(glottodata) |>
      lapply(
        FUN = function(idx){
          tbl <- glottodata[, idx] |>
            table(useNA = "ifany")
          tbl / nrow(glottodata)
        })
  }
  if (is.null(glottodata_val_counts)){
    glottodata_val_counts <- glottodata_freq_list |>
      sapply(length)
  }

  term_1 <- anderberg_dissimilarity.A.B(glottodata = glottodata, glottodata_freq_list = glottodata_freq_list, glottodata_val_counts = glottodata_val_counts,
                                        idx_A = idx_A, idx_B = idx_B, type = type, weights = weights)
  term_2 <- anderberg_dissimilarity.A.B(glottodata = glottodata, glottodata_freq_list = glottodata_freq_list, glottodata_val_counts = glottodata_val_counts,
                                        idx_A = idx_B, idx_B = idx_A, type = type, weights = weights)

  mc <- mean(c(term_1, term_2))
  return(mc)
}

#' Title
#'
#' @param glottosubdata a glottosubdata
#' @param lg1 a language dataframe in glottosubdata
#' @param lg2 a language dataframe in glottosubdata
#'
#' @return a numeric number
#' @noRd
glottodist_anderberg_MC_pairing <- function(glottosubdata, lg1, lg2){
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

  glottodata_freq_list <- 1:ncol(glottodata) |>
    lapply(
      FUN = function(idx){
        tbl <- glottodata[, idx] |>
          table(useNA = "ifany")
        tbl / nrow(glottodata)
      })

  glottodata_val_counts <- glottodata_freq_list |>
    sapply(length) # glottodata_val_counts is a vector containing the amount of different values for each features.

  if ((lg1 %in% glottocodes) && (lg2 %in% glottocodes)) {
    idx_1 <- which(glottocodes == lg1)
    idx_2 <- which(glottocodes == lg2)
  }

  anderberg_dissimilarity.MC(glottodata=glottodata, glottodata_freq_list=glottodata_freq_list,
                             glottodata_val_counts=glottodata_val_counts, idx_A = cnstrn_count[[idx_1]],
                             idx_B = cnstrn_count[[idx_2]], type=type, weights=weights)

}


#' Title
#'
#' @param glottosubdata a glottosubdata
#'
#' @return a numeric number
#' @noRd
glottodist_anderberg_MC <- function(glottosubdata){
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

  glottodata_freq_list <- 1:ncol(glottodata) |>
    lapply(
      FUN = function(idx){
        tbl <- glottodata[, idx] |>
          table(useNA = "ifany")
        tbl / nrow(glottodata)
      })

  glottodata_val_counts <- glottodata_freq_list |>
    sapply(length) # glottodata_val_counts is a vector containing the amount of different values for each features.


  for (i in 1:(dim-1)){
    for(j in (i + 1):dim){
      dist_matrix[i, j] <- anderberg_dissimilarity.MC(glottodata=glottodata, glottodata_freq_list=glottodata_freq_list,
                                                      glottodata_val_counts=glottodata_val_counts,
                                                      idx_A = cnstrn_count[[i]], idx_B = cnstrn_count[[j]], type=type, weights=weights)
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
#' @param glottodata_freq_list a list
#' @param glottodata_val_counts a vector
#' @param idx_A a vector
#' @param idx_B a vector
#' @param type a vector
#' @param weights a vector
#'
#' @return a numeric number
#' @noRd
avg_anderberg <- function(glottodata, glottodata_freq_list = NULL, glottodata_val_counts = NULL,
                          idx_A, idx_B, type, weights){
  # glottodata is a dataframe
  if (is.null(glottodata_freq_list)){
    glottodata_freq_list <- 1:ncol(glottodata) |>
      lapply(
        FUN = function(idx){
          tbl <- glottodata[, idx] |>
            table(useNA = "ifany")
          tbl / nrow(glottodata)
        })
  }
  if (is.null(glottodata_val_counts)){
    glottodata_val_counts <- glottodata_freq_list |>
      sapply(length)
  }

  idx_A |>
    sapply(
      FUN = function(idx_1){
        idx_B |>
          sapply(
            FUN = function(idx_2){
              anderberg_dissimilarity(glottodata=glottodata, glottodata_freq_list = glottodata_freq_list,
                                      glottodata_val_counts = glottodata_val_counts,
                                      i = idx_1, j = idx_2, type = type, weights = weights)
            }) |>
          mean()
      }) |>
    mean()
}

#' Title
#'
#' @param glottodata a dataframe
#' @param glottodata_freq_list a list
#' @param glottodata_val_counts a vector
#' @param idx_A a vector
#' @param idx_B a vector
#' @param avg_idx a vector
#' @param fixed_idx a vector
#' @param type a vector
#' @param weights a vector
#'
#' @return a numeric number
#' @noRd
anderberg_Indexing <- function(glottodata, glottodata_freq_list = NULL, glottodata_val_counts = NULL,
                               idx_A, idx_B, avg_idx, fixed_idx, type, weights){
  # glottodata is a dataframe
  if (is.null(glottodata_freq_list)){
    glottodata_freq_list <- 1:ncol(glottodata) |>
      lapply(
        FUN = function(idx){
          tbl <- glottodata[, idx] |>
            table(useNA = "ifany")
          tbl / nrow(glottodata)
        })
  }
  if (is.null(glottodata_val_counts)){
    glottodata_val_counts <- glottodata_freq_list |>
      sapply(length)
  }

  sum <- 0

  for (s in avg_idx){
    cnstn_1 <- idx_A[which((glottodata[idx_A, s] == "Y") | (glottodata[idx_A, s] == TRUE))]
    cnstn_2 <- idx_B[which((glottodata[idx_B, s] == "Y") | (glottodata[idx_B, s] == TRUE))]

    if (!identical(cnstn_1, integer(0)) && !identical(cnstn_2, integer(0))) {
      avg <- avg_anderberg(glottodata = glottodata[, fixed_idx],
                                glottodata_freq_list = glottodata_freq_list[fixed_idx],
                                glottodata_val_counts = glottodata_val_counts[fixed_idx],
                                weights = weights[fixed_idx],
                                type = type[fixed_idx],
                                idx_A = cnstn_1, idx_B = cnstn_2)
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
#' @noRd
#'
glottodist_anderberg_Indexing <- function(glottosubdata, avg_idx, fixed_idx){
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

  glottodata_freq_list <- 1:ncol(glottodata) |>
    lapply(
      FUN = function(idx){
        tbl <- glottodata[, idx] |>
          table(useNA = "ifany")
        tbl / nrow(glottodata)
      })

  glottodata_val_counts <- glottodata_freq_list |>
    sapply(length) # glottodata_val_counts is a vector containing the amount of different values for each features.

  for (i in 1:(dim-1)){
    for(j in (i + 1):dim){
      dist_matrix[i, j] <- anderberg_Indexing(glottodata=glottodata,
                                        glottodata_freq_list=glottodata_freq_list,
                                        glottodata_val_counts=glottodata_val_counts,
                                        idx_A = cnstrn_count[[i]], idx_B = cnstrn_count[[j]],
                                        avg_idx = avg_idx, fixed_idx = fixed_idx,
                                        type=type, weights=weights)
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






