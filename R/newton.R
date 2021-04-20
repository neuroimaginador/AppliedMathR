#' @export
newton <- function(f, df, x0, n = 10, TOL = 1.e-5) {

    x <- x0

    for (k in seq(n)) {

        Fx <- f(x)

        DFx <- df(x)

        if (DFx == 0) {

            stop("Zero derivative.", call. = FALSE)

        }

        x <- x - (Fx / DFx)


        if (is.nan(x) || is.infinite(x)) {

            stop("NaN", call. = FALSE)
        }

    }

    if (abs(f(x)) >= TOL) {
        warning("Tolerance not achieved.", call. = FALSE)

    }

    return(x)
}
