#' @export
gram_schmidt <- function(A) {

  n <- ncol(A)
  B <- matrix(0, ncol = ncol(A), nrow = nrow(A))

  m <- 1
  while (m <= n) {

    B[, m] <- A[, m]

    if (m > 1) {

      for (i in seq(m - 1)) {

        coef <- sum(B[, m] * B[, i]) / sum(B[, i] ^ 2)

        if (abs(coef) > 1.e-7) {

          B[, m] <- B[, m] - coef * B[, i]

        }

      }

    }

    m <- m + 1

  }

  sums <- matrix((colSums(B ^ 2)),
                 ncol = ncol(B),
                 nrow = nrow(B),
                 byrow = TRUE)

  # B <- B / sums


  return(list(B = B, sums = sums))

}

#' @export
GS <- function(vector_list) {

  sol <- list()

  for (i in seq_along(vector_list)) {

    v <- Vector$new(vector_list[[i]])

    if (i > 1) {

      for (j in seq(i - 1)) {

        w <- Vector$new(vector_list[[j]])
        wv <- w$prod(v)

        if (wv$value() != 0) {

          ww <- w$prod(w)
          coef <- wv$prod(ww$inverse())
          coef$simplify()
          w$prod(coef$prod(-1))

          v$sum(w)

        }

      }

    }

    sol[[i]] <- v

  }

  return(sol)

}