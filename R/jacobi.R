#' @export
jacobi <- function(A, b, n = 100,
                   x0 = 0 * b) {

    D <- diag(diag(A))
    L <- A
    R <- A
    L[!lower.tri(A, diag = FALSE)] <- 0
    R[!upper.tri(A, diag = FALSE)] <- 0
    ID <- solve(D)
    BJ <- -ID %*% (L + R)
    CJ <- ID %*% b

    x <- x0

    for (k in 1:n) {

        x <- BJ %*% x + CJ

    }
    res <- A %*% x - b
    rad <- max(abs(pracma::eig(BJ)))

    return(list(x = x, res = res, rad = rad, BJ = BJ, CJ = CJ))
}
