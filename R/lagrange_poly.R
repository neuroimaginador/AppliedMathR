#' @export
lagrange_poly <- function(xx) {

  n <- length(xx)

  L <- list()
  for (i in seq(n)) {

    x <- xx[-i]
    xi <- xx[i]
    p <- 1
    for (j in seq_along(x)) {

      p <- pracma::polymul(p,
                           c(1, -x[j]) / (xi - x[j]))

    }

    L[[i]] <- p

  }

  return(L)

}
