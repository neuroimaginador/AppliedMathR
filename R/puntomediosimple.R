#' @export
puntomediosimple <- function(F, a, b) {

    h <- (b - a)/2


    I <- 2 * h * feval(F, a + h)


    return(I)
}
