gower.a.b <- function(a, b, wt){
  sum(as.numeric(a != b) * wt) / length(a)
}

gower.a.B <- function(a, B, wt) {
  B |>
    apply(MARGIN = 1, FUN = function(b){
      return(gower.a.b(a=a, b=b, wt=wt))
    }) |>
    min()
}

gower.A.B <- function (A, B, wt) {
  A |>
    apply(MARGIN = 1, FUN = function(a){gower.a.B(a, B, wt)}) |>
    mean()
}

# A function to compute the average of pairwise gower distances with weight wt.
avg_gower <- function(glottodata, idx_A, idx_B, wt){
  # glottodata is a dataframe
  idx_A |>
    sapply(
      FUN = function(idx_1){
        idx_B |>
          sapply(
            FUN = function(idx_2){
              gower.a.b(a = glottodata[idx_1, ], b = glottodata[idx_2, ], wt = wt)
            }) |>
          mean()
      }) |>
    mean()
}

# Compute the MC distance between two point cloud A and B
gower.MC <- function(glottodata, idx_A, idx_B, wt){
  (gower.A.B(A = glottodata[idx_A, ], B = glottodata[idx_B, ], wt = wt) +
     gower.A.B(A = glottodata[idx_B, ], B = glottodata[idx_A, ], wt = wt)) / 2
}

glottodist_gower_MC <- function(glottosubdata){
  glottosubdata_splfy <- glottosimplify(glottosubdata, submerge = F)
  glottocodes <- glottocode_get(glottosubdata_splfy)
  cnstrn_count <- from_to_idx(glottosubdata_splfy |>
                                sapply(nrow))

  params <- glottodist_cleaned(glottodata = glottosubdata, one_level_drop = F)
  glottodata <- params$glottodata
  weights <- params$weights
  type <-  params$type
  type_code <- params$type_code

  dim <- length(glottosubdata_splfy)
  dist_matrix <- matrix(nrow=dim, ncol=dim)

  for (i in 1:(dim-1)){
    for(j in (i + 1):dim){
      dist_matrix[i, j] <- gower.MC(glottodata=glottodata, idx_A = cnstrn_count[[i]],
                                    idx_B = cnstrn_count[[j]], wt=weights)
      dist_matrix[j, i] <- dist_matrix[i, j]
    }
  }

  for (i in 1:dim){
    dist_matrix[i, i] <- 0
  }
  colnames(dist_matrix) <- glottocodes
  rownames(dist_matrix) <- glottocodes
  return(as.dist(dist_matrix))
}

gower_SI <- function(glottodata, idx_A, idx_B, meaning_idx, form_idx, wt){
  # glottodata is a dataframe
  sum_SI <- 0

  for (s in meaning_idx){
    cnstn_1 <- idx_A[which((glottodata[idx_A, s] == "Y") | (glottodata[idx_A, s] == TRUE))]
    cnstn_2 <- idx_B[which((glottodata[idx_B, s] == "Y") | (glottodata[idx_B, s] == TRUE))]

    if (!identical(cnstn_1, integer(0)) && !identical(cnstn_2, integer(0))) {
      avg_form <- avg_gower(glottodata = glottodata[, form_idx],
                            idx_A = cnstn_1, idx_B = cnstn_2,
                            wt = wt[form_idx])
      sum_SI <- sum_SI + avg_form
    }
  }
  return(sum_SI / length(meaning_idx))
}

glottodist_gower_SI <- function(glottosubdata, meaning_idx, form_idx){
  glottosubdata_splfy <- glottosimplify(glottosubdata, submerge = F)
  glottocodes <- glottocode_get(glottosubdata_splfy)
  cnstrn_count <- from_to_idx(glottosubdata_splfy |>
                                sapply(nrow))

  params <- glottodist_cleaned(glottodata = glottosubdata, one_level_drop = F)
  glottodata <- params$glottodata
  weights <- params$weights
  type <-  params$type
  type_code <- params$type_code

  dim <- length(glottosubdata_splfy)
  dist_matrix <- matrix(nrow=dim, ncol=dim)

  for (i in 1:(dim-1)){
    for(j in (i + 1):dim){
      dist_matrix[i, j] <- gower_SI(glottodata=glottodata,
                                    idx_A = cnstrn_count[[i]], idx_B = cnstrn_count[[j]],
                                    meaning_idx = meaning_idx, form_idx = form_idx,
                                    wt = weights)
      dist_matrix[j, i] <- dist_matrix[i, j]
    }
  }

  for (i in 1:dim){
    dist_matrix[i, i] <- 0
  }

  colnames(dist_matrix) <- glottocodes
  rownames(dist_matrix) <- glottocodes
  return(as.dist(dist_matrix))
}

gower_FI <- function(glottodata, idx_A, idx_B, meaning_idx, form_idx, wt){
  # glottodata is a dataframe
  sum_FI <- 0

  for (s in form_idx){
    cnstn_1 <- idx_A[which((glottodata[idx_A, s] == "Y") | (glottodata[idx_A, s] == TRUE))]
    cnstn_2 <- idx_B[which((glottodata[idx_B, s] == "Y") | (glottodata[idx_B, s] == TRUE))]

    if (!identical(cnstn_1, integer(0)) && !identical(cnstn_2, integer(0))) {
      avg_meaning <- avg_gower(glottodata = glottodata[, meaning_idx],
                               idx_A = cnstn_1, idx_B = cnstn_2,
                               wt = wt[meaning_idx])
      sum_FI <- sum_FI + avg_meaning
    }
  }
  return(sum_FI / length(form_idx))
}

glottodist_gower_FI <- function(glottosubdata, meaning_idx, form_idx){
  glottosubdata_splfy <- glottosimplify(glottosubdata, submerge = F)
  glottocodes <- glottocode_get(glottosubdata_splfy)
  cnstrn_count <- from_to_idx(glottosubdata_splfy |>
                                sapply(nrow))

  params <- glottodist_cleaned(glottodata = glottosubdata, one_level_drop = F)
  glottodata <- params$glottodata
  weights <- params$weights
  type <-  params$type
  type_code <- params$type_code

  dim <- length(glottosubdata_splfy)
  dist_matrix <- matrix(nrow=dim, ncol=dim)

  for (i in 1:(dim-1)){
    for(j in (i + 1):dim){
      dist_matrix[i, j] <- gower_FI(glottodata=glottodata,
                                    idx_A = cnstrn_count[[i]], idx_B = cnstrn_count[[j]],
                                    meaning_idx = meaning_idx, form_idx = form_idx,
                                    wt = weights)
      dist_matrix[j, i] <- dist_matrix[i, j]
    }
  }

  for (i in 1:dim){
    dist_matrix[i, i] <- 0
  }

  colnames(dist_matrix) <- glottocodes
  rownames(dist_matrix) <- glottocodes
  return(as.dist(dist_matrix))
}


lg_form_meaning_count <- function(lg, m_idx, f_idx) {
  # A function to count how many constructions having both meaning m_idx and form f_idx being "Y" or TRUE in language lg.
  # lg is a dataframe, m_idx is an index number, f_idx is an index number.
  meaning_indices <- which(lg[, m_idx] == "Y" | lg[, m_idx] == TRUE)
  form_indices <- which(lg[, f_idx] == "Y" | lg[, f_idx] == TRUE)
  intersect(meaning_indices, form_indices) |>
    length()
}

SIM <- function(lg1, lg2, m_idx, f_idx){
  lg_cnt_1 <- lg_form_meaning_count(lg = lg1, m_idx = m_idx, f_idx = f_idx)
  lg_cnt_2 <- lg_form_meaning_count(lg = lg2, m_idx = m_idx, f_idx = f_idx)

  if (lg_cnt_1 == 0 && lg_cnt_2 == 0){
    result <- 1
  } else {
    result <- min(lg_cnt_1 / lg_cnt_2, lg_cnt_2 / lg_cnt_1)
  }
  return(result)
}

FMI <- function(lg1, lg2, form_idx, meaning_idx) {
  form_idx |>
    sapply(FUN = function(f_idx){
      meaning_idx |>
        sapply(
          FUN = function(m_idx){
            1 - SIM(lg1 = lg1, lg2 = lg2, m_idx = m_idx, f_idx = f_idx)
          }
        ) |>
        mean()
    }) |>
    mean()
}

# A function to compute the distance matrix w.r.t. FMI
glottodist_FMI <- function(glottosubdata, meaning_idx, form_idx){
  glottosubdata_splfy <- glottosimplify(glottosubdata, submerge = F)
  glottocodes <- glottocode_get(glottosubdata_splfy)
  cnstrn_count <- from_to_idx(glottosubdata_splfy |>
                                sapply(nrow))

  params <- glottodist_cleaned(glottodata = glottosubdata, one_level_drop = F)
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
                               meaning_idx = meaning_idx, form_idx = form_idx)
      dist_matrix[j, i] <- dist_matrix[i, j]
    }
  }

  for (i in 1:dim){
    dist_matrix[i, i] <- 0
  }

  colnames(dist_matrix) <- glottocodes
  rownames(dist_matrix) <- glottocodes
  return(as.dist(dist_matrix))
}



















