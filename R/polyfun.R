#' Polynomial function
#'
#' @param p Coefficients of the polynomial
#'
#' @return A function that evaluates the polynomial in given points.
#'
#' @export
#'
polyfun <- function(p) {

  function(x) sapply(x, function(xx) pracma::polyval(p, xx))

}
