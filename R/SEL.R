#' @export
rMatrix <- function(n, m = n,
                    values = -2:2,
                    symmetric = FALSE,
                    no_perms = FALSE,
                    upper = TRUE,
                    diagonal = TRUE,
                    lower = TRUE,
                    nz_diag = FALSE,
                    sdd = FALSE) {

  vals <- sample(values,
                 size = m * n,
                 replace = TRUE)

  M <- matrix(vals, nrow = n, ncol = m)

  if (m == n) {

    if (no_perms) {

      lup <- matlib::LU(M)
      P <- lup$P
      M <- P %*% M

    }

    if (!upper) {

      M[upper.tri(M)] <- 0

    }

    if (!lower) {

      M[lower.tri(M)] <- 0

    }

    if (!diagonal) {

      M <- M - diag(diag(M))

    }

    if (nz_diag) {

      new_diag <- sample(setdiff(values, 0),
                         size = n,
                         replace = TRUE)
      diag(M) <- new_diag

    }

    if (sdd) {

      M <- make_strict_dominant_diagonal(M)

    }

    if (symmetric) {

      M <- 0.5 * (M + t(M))

    }

  }

  return(M)

}

#' @export
rVector <- function(n, values = -2:2) {

  vals <- sample(values,
                 size = n,
                 replace = TRUE)

  return(matrix(vals, nrow = n, ncol = 1))

}

#' @export
rSystem <- function(n,
                    ensure_determined = TRUE,
                    no_perms = TRUE,
                    values_A = -2:2,
                    values_x = -2:2,
                    symmetric = FALSE,
                    upper = TRUE,
                    diagonal = TRUE,
                    lower = TRUE,
                    nz_diag = FALSE,
                    sdd = FALSE) {

  if (no_perms) ensure_determined <- TRUE

  repeat({

    A <- rMatrix(n = n,
                 values = values_A,
                 symmetric = symmetric,
                 no_perms = no_perms,
                 upper = upper,
                 diagonal = diagonal,
                 lower = lower,
                 nz_diag = nz_diag,
                 sdd = sdd)

    if (ensure_determined) {

      if (matlib::Det(A) != 0) break

    }

  })

  x <- rVector(n = n, values = values_x)

  b <- A %*% x

  c(A, b) %<-% remove_fraction(A, b)

  res <- list(A = A, x = x, b = b)
  class(res) <- "rSystem"

  return(res)

}

#' @export
print.rSystem <- function(x, ...) {

  res <- write_system(x$A, x$b)
  cat(res, sep = "\n")

}

.S3methods("print", "rSystem")

#' @export
to_latex <- function(x, ...) UseMethod("to_latex")

#' @export
to_latex.rSystem <- function(x, ...) {

  res <- write_system(x$A, x$b, latex = TRUE)
  res <- paste0("\\ensuremath{", res, "}")

  cat(res, sep = "\n")

  return(invisible(res))

}

.S3methods("to_latex", "rSystem")

#' @export
to_latex.matrix <- function(x, ...) {

  dots <- list(...)
  if ("fractions" %in% names(dots)) {

    if (dots$fractions) {

      x <- to_fraction(x, latex = TRUE)

    }

  }

  str <- sapply(seq(nrow(x)), function(r) {

    stringr::str_flatten(x[r, ],
                         collapse = " & ")

  })
  str <- stringr::str_flatten(str, collapse = "\\\\\n")

  header <- paste0("\\left(\\begin{array}{",
                   paste0(rep("c", ncol(x)), collapse = ""), "}\n",
                   collapse = "")

  str <- paste0(header, str, "\n\\end{array}\\right)\n",
                collapse = "")

  # cat(str, sep = "\n")
  return(invisible(str))

}

.S3methods("to_latex", "matrix")

#' @export
jacobi_matrix <- function(A, b) {

  D <- diag(diag(A))
  L <- A
  R <- A
  L[!lower.tri(A, diag = FALSE)] <- 0
  R[!upper.tri(A, diag = FALSE)] <- 0
  ID <- solve(D)
  BJ <- -ID %*% (L + R)
  CJ <- ID %*% b

  return(list(B = BJ, C = CJ))

}

#' @export
gseidel_matrix <- function(A, b) {

  D <- diag(diag(A))
  L <- A
  R <- A
  L[!lower.tri(A, diag = FALSE)] <- 0
  R[!upper.tri(A, diag = FALSE)] <- 0
  ID <- solve(L + D)
  BGS <- -ID %*% R
  CGS <- ID %*% b

  return(list(B = BGS, C = CGS))

}

#' @export
make_strict_dominant_diagonal <- function(M) {

  sums <- rowSums(abs(M)) - diag(abs(M))
  sums <- sums + sample(1:3, size = length(sums), replace = TRUE)

  diag(M) <- sums

  return(M)

}

apply_iter <- function(B, C, x0 = 0 * C, n_iter) {

  x <- x0

  for (i in seq(n_iter)) {

    x <- B %*% x + C

  }

  return(x)

}

convergence_iter <- function(B) {

  max(abs(eigen(B)$values)) < 1

}


#' @export
write_system <- function(A, b,
                         latex = FALSE,
                         fractions = FALSE,
                         format,
                         vars) {

  n <- nrow(A)
  m <- ncol(A)

  if (fractions) {

    A_chr <- to_fraction(A, latex = latex)

  } else {

    A_chr <- matrix(as.character(A),
                    ncol = m, nrow = n)

  }

  if (missing(b)) {

    rhs <- rep("", nrow(A))
    format_rhs <- ""

  } else {

    if (fractions) {

      b_chr <- to_fraction(b, latex = latex)

    } else {

      b_chr <- matrix(as.character(b),
                      ncol = m, nrow = n)

    }

    if (!latex) {

      rhs <- paste0(" = ", b_chr)

    } else {

      rhs <- paste0(" & = & ", b_chr)

    }

    format_rhs <- "cr"

  }

  if (missing(format)) {

    format <- paste0(rep("r", m), collapse = "")

  } else {

    if ((stringr::str_length(format) != 1) & (stringr::str_length(format) != m) & latex) {

      stop("Bad formatting.", call. = FALSE)

    }

  }

  if (missing(vars)) {

    unknowns <- c("x", "y", "z", "t", "u", "v", "w")

    if (n <= 7) {

      unknowns <- unknowns[1:m]

    } else {

      if (latex) {

        unknowns <- paste0("x_{", seq(m), "}")

      } else {

        unknowns <- paste0("x", seq(m))

      }

    }

  } else {

    unknowns <- vars

  }

  if (latex) {

    str <- c(paste0("\\begin{array}{",
                    format, format_rhs, "}",
                    collapse = ""))

  } else {

    str <- c()

  }

  for (i in seq(n)) {

    v <- A[i, ]
    unk <- unknowns
    unk[v == 0] <- ""
    idx <- which(v != 0)
    v2 <- v[idx]
    idx_pos <- which(v2[-1] > 0)
    # vc <- as.character(v2)
    vc <- A_chr[i, idx]
    vc[v2 == 1] <- ""
    vc[v2 == -1] <- "-"
    if (length(idx_pos) > 0) {

      vc[idx_pos + 1] <- paste0("+ ",
                                vc[idx_pos + 1])

    }

    # if (length(vc) == 0) {
    #
    #   vc <- rep("0", m)
    #   idx <- seq(m)
    #
    # }

    if (length(vc) > 0) {

      if (!latex) {

        str <- c(str,
                 paste0(
                   paste0(vc, unknowns[idx],
                          collapse = " "),
                   rhs[i],
                   collapse = " "))

      } else {

        vr <- rep("", m)
        vr[idx] <- vc

        if (stringr::str_length(format) == 1) {

          row <- stringr::str_flatten(paste0(vr, unk),
                                      collapse = "")

        } else {

          row <- stringr::str_flatten(paste0(vr, unk),
                                      collapse = " & ")

        }

        row <- paste0(row, rhs[i], "\\\\")
        str <- c(str, row)

      }

    }

  }

  if (latex) {

    str <- c(str,
             "\\end{array}")

  }

  # cat(str, sep = "\n")

  return(stringr::str_flatten(str, collapse = "\n"))

}


#' @export
vectors_to_latex <- function(M) {

  str <- sapply(seq(ncol(M)),
         function(i) {

           m <- matrix(M[, i], ncol = 1)
           glue_matrices(m,
                         latex = TRUE, fractions = TRUE)

         })

  stringr::str_flatten(str, collapse = ", ")

}
