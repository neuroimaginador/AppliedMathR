#' @export
remove_fraction <- function(..., byrow = TRUE) {

  dots <- list(...)
  L <- length(dots)

  if (L == 1) {

    A <- dots[[1]]
    split_after <- FALSE

  } else {

    A <- do.call(cbind, dots)
    ncols <- sapply(dots, ncol)
    split_after <- TRUE

  }


  if (byrow) {

    s <- lapply(seq(nrow(A)),
                function(i) {

                  v <- as.vector(fractional::denominators(A[i, ]))
                  L <- numbers::mLCM(v)
                  return(A[i, ] * L)

                }
    )

    A <- do.call(rbind, s)

  } else {

    v <- as.vector(fractional::denominators(A))
    L <- numbers::mLCM(v)
    A <- A * L

  }

  if (split_after) {

    A <- split_matrix(A, ncols)

  }


  return(A)

}