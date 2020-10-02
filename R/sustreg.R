#' @export
sustreg <- function(A, b) {

  c(m, n) %<-% size(A)
  c(m1, n1) %<-% size(b)

    if (m != n) {
        stop("Bad dimensions.", call. = FALSE)
    }
    if (n != m1) {
        stop("Bad dimensions.", call. = FALSE)
    }

    x <- matlab::zeros(size(b))
    for (k in seq(n, 1, by = -1)) {

        x[k, ] <- b[k, ]/A[k, k]

        for (t in 1:k - 1) {

            b[t, ] <- b[t, ] - A[t, k] * x[k, ]

        }

    }
    return(x)
}
