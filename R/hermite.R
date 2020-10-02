#' @export
hermite <- function(x, y, dy) {

    n <- length(x)

    xx <- c(x, x)

    yy <- c(y, y)

    ii <- order(xx)
    xx <- xx[ii]
    yy <- yy[ii]
    # c(xx, ii) <- sort(xx) yy <- yy(ii)


    z <- zeros(2 * n, 2 * n)

    z[, 1] <- t(yy)


    for (col in 2:(2 * n)) {

        for (fil in 1:(2 * n - col + 1)) {

            if (xx[fil + col - 1] != xx[fil]) {

                z[fil, col] <- (z[fil + 1, col - 1] - z[fil,
                  col - 1])/(xx[fil + col - 1] - xx[fil])


            }

        }

        if (col == 2) {

            z[seq(1, 2 * n, by = 2), 2] <- t(dy)


        }

    }

    coef <- z[1, ]


    m <- length(xx)

    p <- coef[m]


    for (k in seq(m - 1, 1, by = -1)) {

        p <- conv(p, c(1, -xx[k]))


        p[length(p)] <- p[length(p)] + coef[k]


    }

    return(p)
}
