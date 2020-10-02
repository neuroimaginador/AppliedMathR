#' @export
solve_homogeneous <- function(A) {

  c(n, m) %<-% dim(A)

  if (n == 1) {

    M <- A

  } else {

    x <- gauss_elimination(A,
                           jordan = TRUE, diag1 = TRUE)
    M <- x$U

  }

  M[abs(M) < 1.e-7] <- 0

  if (n == 1) {

    pivots_idx <- which(M != 0)[1]
    params_idx <- setdiff(seq(m),
                          pivots_idx)

  } else {

    pivots_idx <- apply(M, 1, function(r) which(r != 0)[1])
    pivots_idx_final <- pivots_idx[!is.na(pivots_idx)]

    params_idx <- setdiff(seq(m), pivots_idx_final)

  }

  if (length(params_idx) == 0) {

    # Compatible system
    return(matrix(0, nrow = m, ncol = 1))

  }

  G <- matlab::zeros(m, length(params_idx))

  for (k in seq_along(params_idx)) {

    # Filas en las que el parámetro está
    par_idx <- which(M[, params_idx[k]] != 0)

    if (length(par_idx) > 0) {

      for (s in par_idx) {

        G[pivots_idx[s], k] <- -M[s, params_idx[k]] / M[s, pivots_idx[s]]

      }

    }

    G[params_idx[k], k] <- 1

  }

  return(G)

}