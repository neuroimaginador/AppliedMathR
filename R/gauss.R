#' @export
gauss <- function(A, B) {

    c(m, n) %<-% size(A)

    c(m1, n1) %<-% size(B)

    if ((m != n) | (m1 != m)) {

        stop("Bad dimensions.", call. = FALSE)

    }

    C <- cbind(A, B)

    for (k in 1:(m - 1)) {

        if (C[k, k] == 0) {

            ii <- which.max(abs(C[(k + 1):m, k]))
            ii <- ii + k

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
