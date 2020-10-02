#' @export
deriv <- function(F, x0, h) {

    d <- (feval(F, x0 + h) - feval(F, x0 - h))/(2 * h)

    return(d)

}  # (function)
