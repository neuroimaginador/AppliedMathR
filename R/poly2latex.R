#' @export
poly2latex <- function(p, var = "x",
                       fractions = TRUE,
                       is_matrix = FALSE) {

  if (fractions) {

    p_str <- to_fraction(p, latex = TRUE)

  } else {

    p_str <- as.character(p)

  }

  idx1 <- which(p[-length(p)] == 1)
  idx_1 <- which(p[-length(p)] == -1)

  p_str[idx1] <- ""
  p_str[idx_1] <- "-"

  n <- length(p) - 1
  with_power <- p_str[seq(n - 1)]
  with_x <- p_str[n]
  ind_term <- p_str[length(p)]
  ind_var <- ifelse(is_matrix, "I", "")

  terms <- c(glue::glue("[with_power][var]^{[seq(n, 2)]}",
                       .open = "[", .close = "]"),
             glue::glue("{with_x}{var}"),
             glue::glue("{ind_term}{ind_var}"))

  T1 <- terms[1]
  Trem <- terms[-1]
  prem <- p[-1]

  Trem[prem > 0] <- paste0("+", Trem[prem > 0])

  terms <- c(T1, Trem)

  polynomial <- terms[p != 0] %>% str_flatten()

  return(polynomial)

}