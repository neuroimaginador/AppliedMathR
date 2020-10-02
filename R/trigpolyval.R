#' @export
trigpolyval <- function(a, x) {

    n2 <- length(a)
    n <- n2/2

    y <- a[1] + a[n2] * cos(n * x)

    for (k in 1:(n - 1)) {

        y <- y + a[2 * k] * cos(k * x) + a[2 * k + 1] * sin(k * x)

    }  # (for k)

    return(pracma::Real(y))

}  # (function)