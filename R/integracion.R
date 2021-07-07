#' @export
trapeciosimple <- function(f, a, b) {

  (f(a) + f(b)) * (b - a) / 2

}

#' @export
trapeciocompuesto <- function(f, a, b, n) {

  h <- (b - a) / n
  I <- 0
  for (i in seq(n)) {

    I <- I + trapeciosimple(f,
                            a + (i - 1) * h,
                            a + i * h)

  }

  return(I)

}

#' @export
simpson3octavossimple <- function(f, a, b) {

  h <- (b - a) / 3
  x1 <- a + h
  x2 <- a + 2*h

  3*h/8 * (f(a) + f(b) + f(x1) + f(x2))

}

#' @export
simpson3octavoscompuesto <- function(f, a, b, n) {

  h <- (b - a) / n
  I <- 0
  for (i in seq(n)) {

    I <- I + simpson3octavossimple(f,
                                   a + (i - 1) * h,
                                   a + i * h)

  }

  return(I)

}

#' @export
abierta2puntossimple <- function(f, a, b) {

  h <- (b - a) / 3
  x0 <- a + h
  x1 <- a + 2*h
  return(3*h/2 * (f(x0) + f(x1)))
}

#' @export
abierta2puntoscompuesta <- function(f, a, b, n) {

  h <- (b - a) / n
  I <- 0
  for (i in seq(n)) {

    I <- I + abierta2puntossimple(f,
                                  a + (i - 1) * h,
                                  a + i * h)

  }

  return(I)

}
