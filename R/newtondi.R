#' @export
#' @importFrom pracma zeros conv
newtondi <- function(x, y) {

    m <- length(x)

    d <- pracma::zeros(m)

    d[, 1] <- matrix(y, ncol = 1)

    for (k in 2:m) {

        for (l in 1:(m - k + 1)) {

            d[l, k] <- (d[l + 1, k - 1] - d[l, k - 1])/(x[l +
                                                              k - 1] - x[l])

        }

    }

    coef <- d[1, ]

    p <- coef[m]

    for (k in seq(m - 1, 1, by = -1)) {

        p <- pracma::conv(p, c(1, -x[k]))

        p[length(p)] <- p[length(p)] + coef[k]

    }

    return(p)
}

#' @export
#' @importFrom pracma zeros
newtondi_table <- function(x, y) {

    m <- length(x)

    d <- pracma::zeros(m)

    d[, 1] <- matrix(y, ncol = 1)

    for (k in 2:m) {

        for (l in 1:(m - k + 1)) {

            d[l, k] <- (d[l + 1, k - 1] - d[l, k - 1])/(x[l +
                                                              k - 1] - x[l])

        }

    }

    return(d)

}

#' @export
newtondi2poly <- function(x, d) {

    m <- length(x)

    coef <- d[1, ]

    p <- coef[m]

    for (k in seq(m - 1, 1, by = -1)) {

        p <- pracma::conv(p, c(1, -x[k]))

        p[length(p)] <- p[length(p)] + coef[k]

    }

    return(p)

}

#' @export
newtondi_print <- function(x, d, var = "x",
                           fractions = TRUE, latex = TRUE) {

    coef <- d[1, ]
    xx <- x[-length(x)]
    signo <- sign(xx)
    signo2 <- ifelse(signo > 0, " - ", " + ")
    signo2[xx == 0] <- ""
    xx <- abs(xx)

    if (fractions) {

        xx2 <- to_fraction(xx, latex = latex)

    } else {

        xx2 <- as.character(xx)

    }
    xx2[xx == 0] <- ""
    ldeco_base <- ifelse(latex, "\\left(", "(")
    rdeco_base <- ifelse(latex, "\\right)", ")")
    ldeco <- ifelse(xx > 0, ldeco_base, "")
    rdeco <- ifelse(xx > 0, rdeco_base, "")
    x_xx <- paste0(ldeco, var, " ", signo2, " ", xx2, rdeco)

    xxs <- sapply(seq(length(coef)),
                  function(i) {

                      if (i > 1) {

                          stringr::str_flatten(x_xx[seq(i - 1)],
                                               collapse = ifelse(latex,
                                                                 " \\cdot ",
                                                                 " "))

                      } else {

                          ""

                      }

                  })

    signo_coef <- sign(coef)
    coef <- abs(coef)
    signo2_coef <- ifelse(signo_coef < 0, " - ", " + ")
    # signo2_coef[coef == 0] <- ""
    if (coef[1] < 0) {

        signo2_coef[1] <- ""
        coef[1] <- -coef[1]

    }

    if (fractions) {

        coef2 <- to_fraction(coef, latex = latex)

    } else {

        coef2 <- as.character(coef)
    }

    # stringr::str_flatten(c(
    #     coef2[1],
    #     paste0(signo2_coef[-1], coef2[-1],
    #            " ",
    #            xxs[-1])),
    #     collapse = ""
    # ) %>% stringr::str_replace_all("\\s+", " ")

    c(
        coef2[1],
        paste0(signo2_coef[-1], coef2[-1],
               " ",
               xxs[-1]))

}

#' @export
newtondi_tablelatex <- function(x, d, vars = c("x", "y")) {

    n <- nrow(d)
    d <- to_fraction(d, latex = TRUE)

    d[] <- paste0("$", d, "$")

    R <- matrix("", nrow = 2 * n - 1, ncol = ncol(d))

    for (j in seq(ncol(d))) {

        idx <- seq(n - j + 1)
        offset <- j - 2
        R[offset + 2 * idx, j] <- d[idx, j]

    }
    X <- matrix("", nrow = 2* n - 1, ncol = 1)
    X[2 * seq(n) - 1] <- x
    M <- cbind(X, R)

    tablebody <- apply(M, 1,
          function(row) {

              stringr::str_flatten(row, collapse = " & ")

          }) %>%
        stringr::str_flatten("\\\\[-0.5ex]\n")

    formato <- rep("c", n + 1) %>% stringr::str_flatten("|")
    header <- rep("", n + 1)
    header[1:2] <- paste0("$", vars, "_i$")
    # header[1] <- "$x_i$"
    # header[2] <- "$y_i$"
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
