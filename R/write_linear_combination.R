#' @export
write_linear_combination <- function(M, vars) {

  if (length(vars) < ncol(M)) {

    stop("Bad number of var names.", call. = FALSE)

  }

  str <- c()

  for (i in seq(ncol(M))) {

    m <- matrix(M[, i], ncol = 1)

    str <- c(str, ifelse(i > 1, "+", ""),
             glue::glue("{vars[i]}{to_latex(m, fractions = TRUE)}"),
             .open = "{", .close = "}")

  }

  str <- str %>% str_flatten()

  return(str)


}