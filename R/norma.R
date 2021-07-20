#' @export
norma<-function(v,p){
  if (p==1) {

    return (sum(abs(v)))

  }else{
    if (p==Inf) {
      return(max(abs(v)))
    }else{
    return(sqrt(sum(abs(v^p))))
}
  }
}
