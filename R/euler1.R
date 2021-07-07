#' @export
euler1 <- function(F, xs, y0, n) {

    x0 <- xs[1]

    xf <- xs[2]


    h <- (xf - x0)/n


    x <- seq(x0, xf, by = h)


    y <- zeros(pracma::size(y0, 1), n + 1)

    y[, 1] <- y0


    for (k in 1:n) {

        y[, k + 1] <- y[, k] + h * (feval(F, x[k], y[, k]))


    }

    y <- t(y)

    x <- t(x)


    return(list(x = x, y = y))
}
