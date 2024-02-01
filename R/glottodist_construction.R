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
# lg1 and lg2 are dataframes
avg_gower <- function(lg1, lg2, wt) {
  lg1 |>
    apply(MARGIN = 1, FUN = function(x){
      lg2 |>
        apply(MARGIN = 1, FUN = function(y){
          return(gower.a.b(a=x, b=y,wt=wt))
        })
    }) |>
    mean()
}

lg_form_meaning_count <- function(lg, m_idx, f_idx) {
  meaning_indices <- which(lg[, m_idx] == "Y")
  form_indices <- which(lg[, f_idx] == "Y")

  intersect(meaning_indices, form_indices) |>
    length()
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

  params <- glottodist_cleaned(glottodata = glottosubdata)
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








