#' @export
gauss_elimination <- function(...,
                              pivot = c("none",
                                        "partial",
                                        "total"),
                              jordan = FALSE,
                              diag1 = FALSE) {

  dots <- list(...)
  C <- do.call(cbind, dots)
  ncols <- sapply(dots, ncol)

  if (inherits(C, "matrix")) {

    equals <- function(a, b) a == b

  } else {

    equals <- function(a, b) a %>% y_fn("Equals", b) %>% as_r()

  }

  pivot <- match.arg(pivot)

  m <- nrow(C)
  ind_rows <- seq(m)
  n <- ncol(dots[[1]])
  ind <- seq(n)

  step <- list(split_matrix(C, ncols))
  msg <- "Initial matrix"

  max_iter <- min(c(m, n))
  row <- 1
  pivot_col <- 1

  while (row <= max_iter) {

    M <- matrix(C[row:m, pivot_col:n],
                nrow = m - row + 1,
                ncol = n - pivot_col + 1)
    c(i, j, permute_rows, permute_cols, all_zeros) %<-% next_pivot(M,
                                                                   pivot)

    if (all_zeros) break

    ii <- row + i - 1
    jj <- pivot_col + j - 1

    if (permute_rows) {

      d <- C[row, ]
      C[row, ] <- C[ii, ]
      C[ii, ] <- d

      step <- append(step, list(split_matrix(C, ncols)))
      msg <- append(msg,
                    glue::glue("Permute rows {row} and {ii}."))

      ind_rows[c(row, ii)] <- ind_rows[c(ii, row)]

    }

    if (permute_cols) {

      d <- C[, pivot_col]
      C[, pivot_col] <- C[, jj]
      C[, jj] <- d

      d <- ind[pivot_col]
      ind[pivot_col] <- ind[jj]
      ind[jj] <- d

      step <- append(step, list(split_matrix(C, ncols)))
      msg <- append(msg,
                    glue::glue("Permute columns {pivot_col} and {jj}."))

      jj <- pivot_col

    }

    if (diag1 && (C[row, jj] != 1) && (abs(C[row, jj]) > 1.e-7)) {


      msg <- append(msg,
                    glue::glue("Divide row {row} by {fractional(C[row, pivot_col])}."))

      # C[k, ] <- (C[k, ] / C[k, k]) %>% as_r()
      C[row, ] <- (C[row, ] / C[row, jj])

      step <- append(step, list(split_matrix(C, ncols)))

    }

    to_zero <- setdiff(1:m, 1:row)
    if (jordan) to_zero <- setdiff(1:m, row)

    if (abs(C[row, jj]) > 1.e-7) {

      for (l in to_zero) {

        coef <- C[l, jj]/C[row, jj]


        if (!equals(coef, 0)) {

          C[l, ] <- (C[l, ] - coef * C[row, ])

          step <- append(step, list(split_matrix(C, ncols)))
          msg <- append(msg,
                        glue::glue("Multiply row {row} by {(-fractional(coef))} and add to row {l}."))

        }

      }

    }

    row <- row + 1
    if (permute_cols) {

      pivot_col <- pivot_col + 1

    } else {

      pivot_col <- pivot_col + j

    }

    if (pivot_col > n) break

  }

  splits <- split_matrix(C, ncols)

  elimination <- list(step = step,
                      ind = ind,
                      ind_rows = ind_rows,
                      msg = msg,
                      U = C,
                      splits = splits,
                      ncols = ncols)

  class(elimination) <- "elimination"

  return(elimination)

}

next_pivot <- function(M, pivot) {

  if (!is.matrix(M)) {

    M <- matrix(M, nrow = 1, ncol = length(M))

  }


  if (sum(abs(M)) == 0) {

    return(list(i = 0,
                j = 0,
                permute_rows = FALSE,
                permute_cols = FALSE,
                all_zeros = TRUE))

  }

  if (pivot == "none") {

    j_idx <- which(colSums(abs(M)) > 0)

    if (length(j_idx) > 0) {

      j <- j_idx[1]

      i_idx <- which(M[, j] != 0)
      i <- i_idx[1]
      permute_rows <- i > 1
      permute_cols <- FALSE
      all_zeros <- FALSE

    }

  }

  if (pivot == "partial") {

    j_idx <- which(colSums(abs(M)) > 0)

    if (length(j_idx) > 0) {

      j <- j_idx[1]

      i_idx <- which.max(abs(M[, j]))
      i <- i_idx[1]
      permute_rows <- i > 1
      permute_cols <- FALSE
      all_zeros <- FALSE

    }

  }

  if (pivot == "total") {


    c(i, j) %<-% (which.max(abs(M))[1] %>% arrayInd(.dim = dim(M)))

    permute_rows <- i > 1
    permute_cols <- j > 1
    all_zeros <- FALSE

  }

  return(list(i = i,
              j = j,
              permute_rows = permute_rows,
              permute_cols = permute_cols,
              all_zeros = all_zeros))

}

split_matrix <- function(C, ncols) {

  if (!is.matrix(C)) {

    C <- matrix(C, nrow = 1, ncol = length(C))

  }

  split_ini <- c(1, cumsum(ncols[-length(ncols)]) + 1)
  split_end <- cumsum(ncols)

  splits <- lapply(seq_along(split_ini), function(idx) {

    matrix(C[, split_ini[idx]:split_end[idx]], nrow = nrow(C))

  })

  return(splits)

}

#' @export
print.elimination <- function(x, ...) {

  steps <- x$step
  msg <- x$msg

  for (i in seq_along(steps)) {

    cat(msg[i], "\n")

    glue_matrices(steps[[i]], fractions = TRUE) %>% cat()
    # print(fractional(steps[[i]]))
    # print(steps[[i]])
    cat("\n")

  }

}

#' @export
to_latex.elimination <- function(x, ...) {

  steps <- x$step
  msg <- x$msg

  for (i in seq_along(steps)) {

    cat(msg[i], "\n")

    # to_latex(steps[[i]], fractions = TRUE)
    glue_matrices(steps[[i]], latex = TRUE) %>% cat()
    # print(steps[[i]])
    cat("\n")

  }

}

.S3methods("print", "elimination")
.S3methods("to_latex", "elimination")

# gauss_elimination <- function(...,
#                               pivot = c("none",
#                                         "partial",
#                                         "total"),
#                               jordan = FALSE,
#                               diag1 = FALSE) {
#
#   dots <- list(...)
#   C <- do.call(cbind, dots)
#   ncols <- sapply(dots, ncol)
#
#   if (inherits(C, "matrix")) {
#
#     equals <- function(a, b) a == b
#
#   } else {
#
#     equals <- function(a, b) a %>% y_fn("Equals", b) %>% as_r()
#
#   }
#
#   pivot <- match.arg(pivot)
#
#   m <- nrow(C)
#   ind_rows <- seq(m)
#   n <- ncol(dots[[1]])
#   ind <- seq(n)
#
#   step <- list(split_matrix(C, ncols))
#   msg <- "Initial matrix"
#
#   for (k in seq(min(c(m, n)))) {
#
#     if (k < m) {
#
#       if (pivot == "none") {
#
#         if (equals(C[k, k], 0)) {
#
#           ii <- which.max(abs(C[(k + 1):m, k]))
#           ii <- ii + k
#
#           d <- C[k, ]
#           C[k, ] <- C[ii, ]
#           C[ii, ] <- d
#
#           step <- append(step, list(split_matrix(C, ncols)))
#           msg <- append(msg,
#                         glue::glue("Permute rows {k} and {ii}."))
#
#           ind_rows[c(k, ii)] <- ind_rows[c(ii, k)]
#
#         }
#
#       }
#
#       if (pivot == "partial") {
#
#         ii <- which.max(abs(C[k:m, k]))
#         ii <- ii + k - 1
#
#         if (ii != k) {
#           d <- C[k, ]
#           C[k, ] <- C[ii, ]
#           C[ii, ] <- d
#
#           step <- append(step, list(split_matrix(C, ncols)))
#           msg <- append(msg,
#                         glue::glue("Permute rows {k} and {ii}."))
#
#           ind_rows[c(k, ii)] <- ind_rows[c(ii, k)]
#
#         }
#
#       }
#
#       if (pivot == "total") {
#
#         jj <- which.max(apply(abs(C[k:m, k:n]), 2, max))
#         jj <- jj + k - 1
#
#         if (jj != k) {
#           d <- C[, k]
#           C[, k] <- C[, jj]
#           C[, jj] <- d
#
#           d <- ind[k]
#           ind[k] <- ind[jj]
#           ind[jj] <- d
#
#           step <- append(step, list(split_matrix(C, ncols)))
#           msg <- append(msg,
#                         glue::glue("Permute columns {k} and {jj}."))
#
#
#         }
#
#         ii <- which.max(abs(C[k:m, k]))
#         ii <- ii + k - 1
#
#         if (ii != k) {
#           d2 <- C[k, ]
#           C[k, ] <- C[ii, ]
#           C[ii, ] <- d2
#
#           step <- append(step, list(split_matrix(C, ncols)))
#           msg <- append(msg,
#                         glue::glue("Permute rows {k} and {ii}."))
#
#           ind_rows[c(k, ii)] <- ind_rows[c(ii, k)]
#
#         }
#
#       }
#
#     }
#
#     # if (diag1 && (!equals(C[k, k], 1)) && (!equals(C[k, k], 0))) {
#
#     if (diag1 && (C[k, k] != 1) && (abs(C[k, k]) > 1.e-7)) {
#
#       # if (inherits(C[k, k], "yac_symbol")) {
#       #
#       #   c1 <- C[k, k]
#       #
#       # } else {
#       #
#       #   c1 <- as.character(fractional(C[k, k]))
#       #
#       # }
#
#       msg <- append(msg,
#                     glue::glue("Divide row {k} by {fractional(C[k, k])}."))
#
#       # C[k, ] <- (C[k, ] / C[k, k]) %>% as_r()
#       C[k, ] <- (C[k, ] / C[k, k])
#
#       step <- append(step, list(split_matrix(C, ncols)))
#
#     }
#
#     to_zero <- setdiff(1:m, 1:k)
#     if (jordan) to_zero <- setdiff(1:m, k)
#
#     if (abs(C[k, k]) > 1.e-7) {
#
#       for (l in to_zero) {
#
#         coef <- C[l, k]/C[k, k]
#
#         # if (inherits(coef, "yac_symbol")) {
#         #
#         #   c1 <- coef
#         #
#         # } else {
#         #
#         #   c1 <- fractional(coef)
#         #
#         # }
#
#         if (!equals(coef, 0)) {
#
#           # C[l, ] <- (C[l, ] - coef * C[k, ]) %>% as_r()
#           C[l, ] <- (C[l, ] - coef * C[k, ])
#
#           step <- append(step, list(split_matrix(C, ncols)))
#           msg <- append(msg,
#                         glue::glue("Multiply row {k} by {(-fractional(coef))} and add to row {l}."))
#
#         }
#
#       }
#
#     }
#
#   }
#
#   splits <- split_matrix(C, ncols)
#
#   elimination <- list(step = step,
#                       ind = ind,
#                       ind_rows = ind_rows,
#                       msg = msg,
#                       U = C,
#                       splits = splits,
#                       ncols = ncols)
#
#   class(elimination) <- "elimination"
#
#   return(elimination)
#
# }