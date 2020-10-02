#' @export
to_fraction <- function(A, latex = FALSE) {

  if (is.character(A)) return(A)

  A[abs(A) < 1.e-7] <- 0

  if (!latex) {

      A[] <- fractional::vfractional(A)

     return(A)

  }

  den <- fractional::denominators(A)
  num <- fractional::numerators(A)

  A_chr <- A
  A_chr[] <- as.character(A)
  idx <- which(den != 1)
  idx_neg <- which((num < 0) & (den != 1))
  num <- abs(num)

  A_chr[idx] <- paste0("\\frac{", num[idx], "}{", den[idx], "}")
  A_chr[idx_neg] <- paste0("- ", A_chr[idx_neg])

  return(A_chr)

}