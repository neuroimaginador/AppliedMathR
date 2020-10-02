#' @export
newtondi <- function(x, y) {

    m <- length(x)

    d <- zeros(m)

    d[, 1] <- t(y)


    for (k in 2:m) {

        for (l in 1:(m - k + 1)) {

            d[l, k] <- (d[l + 1, k - 1] - d[l, k - 1])/(x[l +
                k - 1] - x[l])


        }

    }

    coef <- d[1, ]


    p <- coef[m]


    for (k in seq(m - 1, 1, by = -1)) {

        p <- conv(p, c(1, -x[k]))


        p[length(p)] <- p[length(p)] + coef[k]


    }

    return(p)
}
