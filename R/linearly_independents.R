#' @export
linearly_independents <- function(gen, original = TRUE) {

  if (ncol(gen) == 1) {

    if (sum(abs(gen)) > 0) {

      return(gen)

    } else {

      return(gen[, -1])

    }

  }

  x <- gauss_elimination(t(gen))
  ind <- x$ind_rows
  s <- x$U

  dependents <- which(rowSums(abs(s)) < 1.e-7)
  gen <- gen[, ind]

  if (!original) gen <- t(s)

  if (length(dependents) > 0) {

    res <- gen[, -dependents]
    if (length(dependents) == ncol(gen) - 1)
      res <- matrix(res, ncol = 1)

  } else {

    res <- gen

  }

  return(res)

}
