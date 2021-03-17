#' @export
get_unknowns <- function(m, latex = FALSE) {

  unks <- c("x", "y", "z", "t", "u", "v", "w")

  if (m <= 7) {

    unks <- unks[1:m]

  } else {

    if (latex) {

      unks <- paste0("x_{", seq(m), "}")

    } else {

      unks <- paste0("x", seq(m))

    }

  }

  return(unks)

}