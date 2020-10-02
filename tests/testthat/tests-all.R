# Tests
library(optR)
library(Rlinsolve)
library(matlab)
library(matlib)
library(fractional)
library(matconv)
library(magrittr)
library(stringr)
library(purrr)
library(pracma)

# ENL
## bipart
f <- function(x) x ^ 2 - 3
x <- bipart(f, 1, 2, n = 10)
abs(f(x))

# newton
df <- function(x) 2 * x
x <- numericoUMA::newton(f, df, x0 = 2)
abs(f(x))

# SEL
## Gauss
s <- rSystem(5)
x <- gauss(s$A, s$b)
norm(x - s$x)

## Gauss parcial
x <- gausspar(s$A, s$b)
norm(x - s$x)

## Gauss total
x <- gausstot(s$A, s$b)
norm(x - s$x)

## Gauss-Jordan
x <- gaussjor(s$A, s$b)
norm(x - s$x)

## LU
x <- doolittle(s$A, s$b)
norm(x - s$x)

## Cholesky
s <- rSystem(5, symmetric = TRUE)
A <- make_strict_dominant_diagonal(s$A)
x <- cholesky(A, s$b)
norm(A %*% x - s$b)

## QR
x <- solqr(s$A, s$b)
norm(x - s$x)

## Jacobi
repeat({

  s <- rSystem(5, nz_diag = TRUE)
  c(B, C) := jacobi_matrix(s$A, s$b)

  if (convergence_iter(B)) break

})
R <- jacobi(s$A, s$b)
norm(R$x - s$x)

## Gauss-Seidel
repeat({

  s <- rSystem(5, nz_diag = TRUE)
  c(B, C) := gseidel_matrix(s$A, s$b)

  if (convergence_iter(B)) break

})
R <- gseidel(s$A, s$b)
norm(R$x - s$x)

# Interpolación
p_real <- c(2, 3, 4, 5, -1, 0, 6)
x <- runif(n = length(p_real))
y <- polyval(p_real, x)

## polyfit
p_polyfit <- polyfit(x, y, n = length(x) - 1)
sum(abs(p_polyfit - p_real))

## newtondi
p_newtondi <- newtondi(x, y)
sum(abs(p_polyfit - p_real))

## hermite
p_deriv <- polyder(p_real)
dy <- polyval(p_deriv, x)
p_hermite <- hermite(x, y, dy)
sum(abs(polyval(p_hermite, x) - y))
sum(abs(polyval(p_deriv, x) - dy))

# Aproximación
## polyfit
recta <- polyfit(x, y, 1)
parabola <- polyfit(x, y, 2)

## trigonométrica
y <- c(1, -1, 2, 4)
x <- linspace(0, 2*pi, n = length(y) + 1)
x <- x[-length(x)]
c <- fft(y)
a <- fft2poly(c)
y_est <- trigpolyval(a, x)
sum(abs(y_est - y))

# Derivación e integración
## Deriv
## simpson simple y compuesto
## ncotes5 simple y compuesto
## punto medio simple y compuesto
