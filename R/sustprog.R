#' @export
sustprog <- function(A, b) {

    c(m, n) %<-% pracma::size(A)
    c(m1, n1) %<-% pracma::size(b)

    if (m != n) {
        stop("Bad dimensions.", call. = FALSE)
    }
    if (n != m1) {
        stop("Bad dimensions.", call. = FALSE)
    }

    x <- matlab::zeros(pracma::size(b))

    for (k in 1:n) {

        x[k, ] <- b[k, ]/A[k, k]

        if (k < n) {

            for (t in (k + 1):n) {

                b[t, ] <- b[t, ] - A[t, k] * x[k, ]

            }

        }


    }
    return(x)
}
