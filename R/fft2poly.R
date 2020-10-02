#' @export
fft2poly <- function(c) {

    n2 <- length(c)
    n <- n2/2

    a <- matlab::zeros(1, n2)
    a[1] <- c[1]/n2
    a[n2] <- c[1 + n]/n2

    for (k in 1:(n - 1)) {

        a[2 * k] <- (c[1 + k] + c[1 + n2 - k])/n2
        a[2 * k + 1] <- (0+1i) * (c[1 + k] - c[1 + n2 - k])/n2

    }  # (for k)

    return(Real(a))

}  # (function)
