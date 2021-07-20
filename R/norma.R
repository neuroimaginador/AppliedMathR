#' @export
norma <- function(v, p) {
  if (p == 1) {
    return(sum(abs(v)))
  }
  if (p == Inf) {
    return(max(abs(v)))
  } else {
    return("aun no definida")
  }
}
