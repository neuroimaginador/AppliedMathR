#' @export
doolittle <- function(A,b){

  lup <- matlib::LU(A)
  L <- lup$L
  U <- lup$U
  P <- lup$P
  bb <- P %*% b
  y <- sustprog(L, bb)
  x <- sustreg(U, y)

  return(x)

}
