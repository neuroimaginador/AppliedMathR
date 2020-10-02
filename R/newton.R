#' @export
newton <- function(F, DF, x0, n = 10, TOL = 1.e-5) {

    x <- x0

    e <- 1


    for (k in 1:n) {

        Fx <- feval(F, x)

        DFx <- feval(DF, x)


        if (DFx == 0) {

            stop("Zero derivative.", call. = FALSE)

        }

        x <- x - (Fx/DFx)


        if (is.nan(x) || is.infinite(x)) {

            stop("NaN", call. = FALSE)
        }

    }

    if (abs(feval(F, x)) >= TOL) {
        warning("Tolerance not achieved.", call. = FALSE)

    }

    return(x)
}
