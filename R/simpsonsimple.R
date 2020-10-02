#' @export
simpsonsimple <- function(F, a, b) {

    I <- (feval(F, a) + 4 * feval(F, (a + b)/2) + feval(F,
        b))/6 * (b - a)


    return(I)
}
