#' @export
ncotes5simple <- function(F, a, b) {

    h <- (b - a)/4


    I <- (7 * (feval(F, a) + feval(F, b)) + 32 * (feval(F,
        a + h) + feval(F, a + 3 * h)) + 12 * feval(F, a +
        2 * h))/90 * (b - a)


    return(I)
}
