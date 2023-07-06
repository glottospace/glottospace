#' Calculate Anderberg dissimilarity between two objects
#'
#' @param glottodata A dataframe
#' @param i An integer
#' @param j An integer
#'
#' @return A numeric value
#' @export
#'
#' @noRd
#'
#'
anderberg_dissimilarity <- function(glottodata, i, j, type, weights) {
  glottodata_freq_list <- glottodata |>
    plyr::alply(
      2, .fun = function(x){table(x, useNA = "ifany")}
    ) |>
    lapply (
      FUN = function(x){
        x / sum(x)
      }
    ) # glottodata_freq_list is a list containing the frequencies of each value in each feature.

  glottodata_val_counts <- glottodata_freq_list |>
    sapply(length) # glottodata_val_counts is a vector containing the amount of different values for each features.


  delta <- rep(1, length(weights)) # Define the delta vector
  if (length(type$asymm) != 0){
    delta_0_idx <- intersect(which(glottodata[i, type$asymm] == FALSE), which(glottodata[j, type$asymm] == FALSE))
    delta[delta_0_idx] <- 0
  } # If both values of an asymm type feature are FALSE, delta is set to be 0

  feature_same_idx_na <- intersect(which(is.na(glottodata[i, ])), which(is.na(glottodata[j, ]))) # The indices of features with both values being NAs
  if (length(feature_same_idx_na) != 0){
    delta[feature_same_idx_na] <- 0
  } # If both values of a feature are NAs, delta is set to be 0


  weights <- weights * delta # Upgrade the weight

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
#' @export
#'
#' @noRd
#'
glottodist_anderberg <- function(glottodata, type, weights){
  n <- nrow(glottodata)
  dist_matrix <- matrix(nrow=n, ncol=n)
  for (i in 1:(n-1)){
    for(j in (i + 1):n){
      dist_matrix[i, j] <- anderberg_dissimilarity(glottodata, i, j, type=type, weights=weights)
      dist_matrix[j, i] <- dist_matrix[i, j]
    }
  }

  for (i in 1:n){
    dist_matrix[i, i] <- 0
  }
  colnames(dist_matrix) <- rownames(glottodata)
  rownames(dist_matrix) <- rownames(glottodata)
  return(as.dist(dist_matrix))
}






