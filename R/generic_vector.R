#' @export
generic_vector <- function(sz, latex = TRUE) {

  if (is.matrix(sz))
    n <- nrow(sz)
  else
    n <- sz

  return(matrix(get_unknowns(n, latex = TRUE), ncol = 1))

}

#' @export
zero_vector <- function(sz) {

  if (is.matrix(sz))
    n <- nrow(sz)
  else
    n <- sz

  return(matrix(0, nrow = n, ncol = 1))

}