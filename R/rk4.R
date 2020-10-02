#' @export
rk4 <- function(F, xs, y0, n) {

    x0 <- xs[1]

    xf <- xs[2]


    h <- (xf - x0)/n


    x <- seq(x0, xf, by = h)


    y <- zeros(size(y0, 1), n + 1)

    y[, 1] <- y0


    for (k in 1:n) {

        k1 <- feval(F, x[k], y[, k])

        k2 <- feval(F, x[k] + h/2, y[, k] + h/2 * k1)

        k3 <- feval(F, x[k] + h/2, y[, k] + h/2 * k2)

        k4 <- feval(F, x[k] + h, y[, k] + h * k3)


        y[, k + 1] <- y[, k] + (k1 + k4 + 2 * (k2 + k3)) *
            h/6


    }

    y <- t(y)

    x <- t(x)


    return(list(x = x, y = y))
}
