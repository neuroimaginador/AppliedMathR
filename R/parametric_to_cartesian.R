#' @export
parametric_to_cartesian <- function(P) {

  # Usa el método de eliminación de los parámetros para
  # pasar de ecuaciones paramétricas a cartesianas para
  # un subespacio vectorial

  n <- nrow(P)
  m <- ncol(P)
  A <- pracma::eye(n)

  M <- cbind(P, A)

  res <- list()
  idx <- c()

  # Recorremos los parámetros
  for (i in seq(m)) {

    # Tomamos la primera ecuación en la que aparece ese parámetro
    eq_idx <- which(abs(M[, i]) > 0)

    if (length(eq_idx) > 0) {

      eq_idx <- eq_idx[1]
      idx <- c(idx, eq_idx)

      coef <- M[eq_idx, i]

      # Despejamos el parámetro en dicha ecuación
      eq <- -M[eq_idx, ] / coef

      # Y sustituimos en el resto de ecuaciones
      for (row in seq(nrow(M))) {

        # Coeficiente del parámetro en la ecuación
        coef_eq <- M[row, i]
        M[row, ] <- M[row, ] + coef_eq * eq

      }

      M[abs(M) < 1.e-7] <- 0

      # Eliminando la ecuación de la que hemos despejado
      M <- matrix(M[-eq_idx, ], ncol = ncol(M))

      res <- append(res, list(split_matrix(M, c(m, n))))

    }

    if (max(sum(abs(M[, seq(m)]))) == 0) break

  }

  # Al acabar, tenemos en A la matriz de las ecuaciones cartesianas
  A <- matrix(M[, -seq(m)], ncol = n)

  return(list(A = A,
              splits = res,
              eq_idx = idx))

}