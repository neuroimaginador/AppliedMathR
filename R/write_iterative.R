write_iterative <- function(A, b,
                            latex = TRUE,
                            fractions = TRUE,
                            algo = c("none",
                                     "jacobi",
                                     "gseidel")) {

  coef <- 1 / diag(A)
  L <- -A
  L[upper.tri(L, diag = TRUE)] <- 0
  R <- -A
  R[lower.tri(L, diag = TRUE)] <- 0

  if (fractions) {

    L_chr <- to_fraction(L, latex = latex)
    R_chr <- to_fraction(R, latex = latex)
    coef_chr <- to_fraction(coef, latex = latex)

  } else {

    L_chr <- matrix(as.character(L),
                    ncol = m, nrow = n)
    R_chr <- matrix(as.character(R),
                    ncol = m, nrow = n)
    coef_chr <- matrix(as.character(coef),
                       ncol = 1)

  }

  L_chr[L > 0] <- paste0("+ ", L_chr[L > 0])
  R_chr[R > 0] <- paste0("+ ", R_chr[R > 0])

  L_chr[L == 1] <- "+"
  L_chr[L == -1] <- "-"
  R_chr[R == 1] <- "+"
  R_chr[R == -1] <- "-"

  v <- get_unknowns(m = ncol(A), latex = TRUE)

  algo <- match.arg(algo)

  if (algo == "none") {

    exp_l <- exp_r <- this_iter <- ""

  }

  if (algo == "jacobi") {

    exp_l <- exp_r <- ifelse(latex, "^{(k)}", "^k")
    this_iter <- ifelse(latex, "^{(k+1)}", "^(k+1)")

  }

  if (algo == "gseidel") {

    exp_l <- ifelse(latex, "^{(k+1)}", "^(k+1)")
    exp_r <- ifelse(latex, "^{(k)}", "^(k)")
    this_iter <- ifelse(latex, "^{(k+1)}", "^(k+1)")

  }

  v_l <- paste0(v, exp_l)
  v_r <- paste0(v, exp_r)

  if (fractions) {

    b_chr <- to_fraction(b, latex = latex)

  } else {

    b_chr <- matrix(as.character(b),
                    ncol = 1)

  }

  sep <- ifelse(latex, " & = &", " = ")
  paren_l <- ifelse(latex, "\\left(", "(")
  paren_r <- ifelse(latex, "\\right)", ")")
  str <- ifelse(latex, "\\begin{array}{rcl}\n", "")
  finalizer <- ifelse(latex, "\\\\\n", "\n")
  closing <- ifelse(latex, "\\end{array}\n", "")

  for (i in seq(nrow(A))) {

    r <- paste0(R_chr[i, ], v_r)
    r <- r[R[i, ] != 0]
    if (length(r) == 0) r <- ""
    l <- paste0(L_chr[i, ], v_l)
    l <- l[L[i, ] != 0]
    if (length(l) == 0) l <- ""

    str <- c(str,
             glue::glue("{v[i]}{this_iter}{sep} {coef_chr[i]}{paren_l} {b_chr[i]} {l} {r} {paren_r} {finalizer}"))

  }

  str <- c(str, closing) %>%
    stringr::str_flatten(collapse = "")


  return(str)

}