#' @export
solqr <- function(A, b) {

    c(m, n) %<-% pracma::size(A)

    gs <- qr(A)
    Q <- qr.Q(gs)
    R <- qr.R(gs)


    bb <- t(Q) %*% b

    x <- sustreg(R[1:n, 1:n], bb)

    return(x)
}
