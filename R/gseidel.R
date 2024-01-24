#' @export
gseidel <- function(A, b, n = 100,
                    x0 = 0 * b) {


    D <- diag(diag(A))
    L <- A
    R <- A
    L[!lower.tri(A, diag = FALSE)] <- 0
    R[!upper.tri(A, diag = FALSE)] <- 0
    ID <- solve(L + D)
    BGS <- -ID %*% R
    CGS <- ID %*% b

    x <- x0

    for (k in 1:n) {

        x <- BGS %*% x + CGS

    }

    res <- A %*% x - b
    rad <- max(abs(pracma::eig(BGS)))

    return(list(x = x, res = res, rad = rad, BGS = BGS,
        CGS = CGS))
}
