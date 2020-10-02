#' @export
gausstot <- function(A, B) {

    c(m, n) %<-% size(A)
    c(m1, n1) %<-% size(B)

    e <- 0
    if (m != n) {

        stop("Bad dimensions.", call. = FALSE)

    }
    if (m1 != m) {
        stop("Bad dimensions.", call. = FALSE)
    }
    C <- cbind(A, B)

    ind <- 1:n


    for (k in 1:(m - 1)) {

        jj <- which.max(apply(abs(C[k:m, k:m]), 2, max))
        jj <- jj + k - 1

        if (jj != k) {
            d <- C[, k]
            C[, k] <- C[, jj]
            C[, jj] <- d

            d <- ind[k]
            ind[k] <- ind[jj]
            ind[jj] <- d

        }

        ii <- which.max(abs(C[k:m, k]))
        ii <- ii + k - 1

        if (ii != k) {
            d2 <- C[k, ]
            C[k, ] <- C[ii, ]
            C[ii, ] <- d2
        }

        for (l in (k + 1):m) {

            coef <- C[l, k]/C[k, k]

            C[l, ] <- C[l, ] - coef * C[k, ]

        }
    }

    newA <- C[, 1:m]
    newB <- matrix(C[, (m + 1):(m + n1)], ncol = n1)

    sol <- sustreg(newA, newB)

    sol[ind] <- sol
    x <- sol

    return(x)
}
