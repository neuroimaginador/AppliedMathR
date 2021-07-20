#' @export
norma <- function(v, p) {

  if (p == 1) {

    return(sum(abs(v)))

  } else {

    if (p == Inf) {

      return(max(abs(v)))

    } else {

      if (p > 0) {

        return((sum(abs(v^p)))^(1/p))

      }

      stop("Invalid p", call. = FALSE)

    }
  }
}
