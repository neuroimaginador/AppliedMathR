#' @export
gaussjor <- function(A, B) {

    c(m, n) %<-% size(A)

    c(m1, n1) %<-% size(B)

    if (m != n) {

        stop("Bad dimensions.", call. = FALSE)

    }
    if (m1 != m) {
        stop("Bad dimensions.", call. = FALSE)
    }
    C <- cbind(A, B)

    for (k in 1:m) {

        C[k, ] <- C[k, ]/C[k, k]

        for (l in 1:m) {

            if (l != k) {

                coef <- C[l, k]

                C[l, ] <- C[l, ] - coef * C[k, ]

            }
        }
    }
    x <- C[, (m + 1):(m + n1)]

    return(x)
}
