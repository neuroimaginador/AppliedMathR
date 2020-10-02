#' @export
bipart <- function(F, a, b, n) {

    FA <- feval(F, a)
    FB <- feval(F, b)

    x <- (a + b)/2

    e <- 0

    if (FA * FB > 0) {

        stop("There is no solution in the specified interval.",
             call. = FALSE)
    }

    if (FA == 0) {
        x <- a  # Si f(a) = 0, devolvemos x = a
        return(x)
    }
    if (FB == 0) {
        x <- b  # Si f(b) = 0, devolvemos x = b
        return(x)
    }

    for (k in 1:n) {

        FX <- feval(F, x)

        if (FX == 0) {
            return(x)
        }

        if (FA * FX < 0) {
            b <- x
        } else {
            a <- x
            FA <- feval(F, a)
        }  # (if)

        x <- (a + b)/2

    }  # (for k)

    # tol <- (b - a)/2

    return(x)

}  # (function)
