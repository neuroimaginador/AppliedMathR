#' @export
#' @title First derivative of a function given by a table
deriv_table <- function(x0, xx, yy) {

  ii <- which(xx == x0)
  # If the point is in the left endpoint: uncentered difference
  if (ii == 1) {

    return((yy[2] - yy[1]) / (xx[2] - xx[1]))

  }
  # In the right, left-differences
  if (ii == length(xx)) {

    return((yy[ii] - yy[ii - 1]) / (xx[ii] - xx[ii - 1]))

  }

  # In other cases, centered differences:
  return( (yy[ii + 1] - yy[ii - 1]) / (xx[ii + 1] - xx[ii - 1]))

}

#' @export
second_deriv_table <- function(x0, xx, yy) {

  ii <- which(xx == x0)
  # If the point is in the left endpoint: uncentered difference
  if (ii == 1) {

    x0l <- x0
    x0r <- xx[2]

  } else {

    # In the right, left-differences
    if (ii == length(xx)) {

      x0l <- xx[ii - 1]
      x0r <- x0

    } else {

      # In other cases, centered differences:
      x0l <- xx[ii - 1]
      x0r <- xx[ii + 1]

    }

  }

  # In other cases, centered differences:
  return( (deriv_table(x0r, xx, yy) - deriv_table(x0l, xx, yy)) / (x0r - x0l))

}
