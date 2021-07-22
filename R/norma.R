#' @export
norma <- function(v, p) {

  if (p == 1) {

    return(sum(abs(v)))

  } else {

    if (p == Inf) {

      return(max(abs(v)))

    } else {

      if (p > 0) {

        return((sum(abs(v^p)))^(1/p))

      }

      stop("Invalid p", call. = FALSE)

    }
  }
}

normamat <- function(v,p){
  if (p==1){
    return(max(colSums(abs(A) )))
    }
  else{
    if (p == Inf) {
      return(max(rowSums(abs(A))))
    }else{
      if (p=="Fro"){
        return(sum(A^2))
      }else{
        if (p==2){
          return ( sqrt(max(abs(eigen( t(Conj(A)*A))$values))))
        }else{
  return("No es una p válida")
}}}}}
