#' @export
ncotes5compuesto <- function(F, a, b, n) {

    h <- (b - a)/n


    I <- 0


    for (k in 1:n) {

        I <- I + ncotes5simple(F, a + (k - 1) * h, a + k *
            h)


    }

    return(I)
}
