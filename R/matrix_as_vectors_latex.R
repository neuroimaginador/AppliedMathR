#' @export
matrix_as_vectors_latex <- function(A) {

  apply(A, 2, \(v) {

    v2 <- v |> stringr::str_flatten(" \\\\\n")
    glue::glue(
      "\\begin{{pmatrix}}{v2}\\end{{pmatrix}}"
    )

  }) |>
    stringr::str_flatten_comma()

}
