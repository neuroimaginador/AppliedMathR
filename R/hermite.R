#' @export
hermite <- function(x, y, dy, decimals = 0) {

    n <- length(x)

    xx <- c(x, x)
    if (decimals > 0)
        xx <- xx %>% round(digits = decimals)

    yy <- c(y, y)
    if (decimals > 0)
        yy <- yy %>% round(digits = decimals)

    if (decimals > 0)
        dy <- dy %>% round(digits = decimals)

    ii <- order(xx)
    xx <- xx[ii]
    yy <- yy[ii]

    z <- zeros(2 * n, 2 * n)

    z[, 1] <- t(yy)


    for (col in 2:(2 * n)) {

        for (fil in 1:(2 * n - col + 1)) {

            if (xx[fil + col - 1] != xx[fil]) {

                z[fil, col] <- (z[fil + 1, col - 1] - z[fil,
                                                        col - 1])/(xx[fil + col - 1] - xx[fil])

                if (decimals > 0)
                    z[fil, col] <- z[fil, col] %>% round(digits = decimals)

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

        if (decimals > 0)
            p <- p %>% round(digits = decimals)

        p[length(p)] <- p[length(p)] + coef[k]

    }

    return(p)
}

#' @export
#' @importFrom pracma zeros
hermite_table <- function(x, y, dy, decimals = 0) {

    n <- length(x)

    xx <- c(x, x)

    if (decimals > 0)
        xx <- xx %>% round(digits = decimals)

    yy <- c(y, y)
    if (decimals > 0)
        yy <- yy %>% round(digits = decimals)

    if (decimals > 0)
        dy <- dy %>% round(digits = decimals)


    ii <- order(xx)
    xx <- xx[ii]
    yy <- yy[ii]

    z <- pracma::zeros(2 * n, 2 * n)

    z[, 1] <- t(yy)


    for (col in 2:(2 * n)) {

        for (fil in 1:(2 * n - col + 1)) {

            if (xx[fil + col - 1] != xx[fil]) {

                z[fil, col] <- (z[fil + 1, col - 1] - z[fil,
                                                        col - 1])/(xx[fil + col - 1] - xx[fil])

                if (decimals > 0)
                    z[fil, col] <- z[fil, col] %>% round(digits = decimals)


            }

        }

        if (col == 2) {

            z[seq(1, 2 * n, by = 2), 2] <- t(dy)


        }

    }

    return(z)

}

#' @export
hermite2poly <- function(x, d) {

    xx <- sort(c(x, x))
    newtondi2poly(xx, d)

}

#' @export
hermite_print <- function(x, d,
                           fractions = TRUE, latex = TRUE) {

    xx <- sort(c(x, x))
    newtondi_print(xx, d, fractions = fractions, latex = latex)

}

#' @export
hermite_tablelatex <- function(x, d, fractions = TRUE) {

    xx <- sort(c(x, x))
    n <- nrow(d)
    if (fractions) {

        d <- to_fraction(d, latex = TRUE)

    }

    d[] <- paste0("$", d, "$")

    R <- matrix("", nrow = 2 * n - 1, ncol = ncol(d))

    for (j in seq(ncol(d))) {

        idx <- seq(n - j + 1)
        offset <- j - 2
        R[offset + 2 * idx, j] <- d[idx, j]

    }
    X <- matrix("", nrow = 2 * n - 1, ncol = 1)
    X[2 * seq(n) - 1] <- xx
    M <- cbind(X, R)

    tablebody <- apply(M, 1,
                       function(row) {

                           stringr::str_flatten(row, collapse = " & ")

                       }) %>%
        stringr::str_flatten("\\\\[-0.5ex]\n")

    formato <- rep("c", n + 1) %>% stringr::str_flatten("|")
    header <- rep("", n + 1)
    header[1] <- "$x_i$"
    header[2] <- "$y_i$"
    header[3] <- "$y'_i$"
    header <- header %>% stringr::str_flatten(" & ")

    table <- glue::glue(
        "\\begin{tabular}{[formato]}\n",
        "[header]\\\\\\hline\n",
        "[tablebody]\n",
        "\\end{tabular}",
        .open = "[", .close = "]") %>%
        stringr::str_flatten("\n")

    return(table)
}

