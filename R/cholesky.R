#' @export
cholesky <- function(A, b){
  M <- chol(A) # A=M'M    Ax=M'Mx=b
  y <- sustprog(t(M), b) # M'y=b
  x <- sustreg(M,y) # M x=y

  return(x)
}
