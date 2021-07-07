#' @export
gausspar <- function(A, B) {

    c(m, n) %<-% pracma::size(A)

    c(m1, n1) %<-% pracma::size(B)

    e <- 0
    if (m != n) {

        stop("Bad dimensions.", call. = FALSE)

    }
    if (m1 != m) {
        stop("Bad dimensions.", call. = FALSE)
    }
    C <- cbind(A, B)


    for (k in 1:(m - 1)) {

        ii <- which.max(abs(C[k:m, k]))
        ii <- ii + k - 1

        if (ii != k) {
            d <- C[k, ]
            C[k, ] <- C[ii, ]
            C[ii, ] <- d
        }

        for (l in (k + 1):m) {

            coef <- C[l, k]/C[k, k]

            C[l, ] <- C[l, ] - coef * C[k, ]

        }
    }

    newA <- C[, 1:m]
    newB <- matrix(C[, (m + 1):(m + n1)], ncol = n1)

    x <- sustreg(newA, newB)

    return(x)
}
