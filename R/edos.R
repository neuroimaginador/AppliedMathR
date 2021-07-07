# EDOs

#' @export
rk2puntomedio <- function(f, xi, y0,
                          digits = NULL) {

  w <- numeric(length(xi))
  w[1] <- y0

  h <- xi[2] - xi[1]

  for (i in seq(2, length(xi))) {

    k1 <- h * f(xi[i - 1], w[i - 1])
    k2 <- h * f(xi[i - 1] + 0.5 * h,
                w[i - 1] + 0.5 * k1)

    if (!is.null(digits)) {

      k1 <- k1 %>% round(digits)
      k2 <- k2 %>% round(digits)

    }

    w[i] <- w[i - 1] + k2

  }

  return(w)

}

#' @export
rk2heun <- function(f, xi, y0,
                          digits = NULL) {

  w <- numeric(length(xi))
  w[1] <- y0

  h <- xi[2] - xi[1]

  for (i in seq(2, length(xi))) {

    k1 <- h * f(xi[i - 1], w[i - 1])
    k2 <- h * f(xi[i - 1] + 2 * h / 3,
                w[i - 1] + 2 * k1 / 3)

    if (!is.null(digits)) {

      k1 <- k1 %>% round(digits)
      k2 <- k2 %>% round(digits)

    }

    w[i] <- w[i - 1] + 0.25 * (k1 + 3 * k2)

  }

  return(w)

}

#' @export
rk2eulermodificado <- function(f, xi, y0,
                          digits = NULL) {

  w <- numeric(length(xi))
  w[1] <- y0

  h <- xi[2] - xi[1]

  for (i in seq(2, length(xi))) {

    k1 <- h * f(xi[i - 1], w[i - 1])
    k2 <- h * f(xi[i - 1] + h,
                w[i - 1] + k1)

    if (!is.null(digits)) {

      k1 <- k1 %>% round(digits)
      k2 <- k2 %>% round(digits)

    }

    w[i] <- w[i - 1] + 0.5 * (k1 + k2)

  }

  return(w)

}
