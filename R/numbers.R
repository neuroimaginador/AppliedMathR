#' @export
Number <- R6::R6Class(

  classname = "Number",

  public = list(

    initialize = function(number, num, den) {

      if (!missing(number)) {

        if (inherits(number, "Number")) {

          decomp <- number$decomposition()
          private$num_decomposition <- decomp$num
          private$den_decomposition <- decomp$den
          private$this_value <- number$value()
          private$is_zero <- number$value() == 0

          return(invisible(self))

        } else {

          num <- numerators(number)
          den <- denominators(number)

        }

      } else {

        number <- num / den

      }

      private$this_value <- number
      private$num_decomposition <- unique(rbind(c(sign(num), 1),
                                                sfsmisc::factorize(abs(num))[[1]]))
      private$den_decomposition <- unique(sfsmisc::factorize(den)[[1]])

      if (number == 0) {

        private$is_zero <- TRUE
        private$num_decomposition <- matrix(c(0, 1), ncol = 2)
        private$den_decomposition <- matrix(1, ncol = 2)

      }

      return(invisible(self))

    },

    value = function() {

      if (private$is_zero) return(0)

      decomp2number(private$num_decomposition) / decomp2number(private$den_decomposition)

    },

    simplify = function() {

      if (find_power(private$num_decomposition, 0)) {
        private$is_zero <- TRUE
        private$num_decomposition <- matrix(c(0, 1), ncol = 2)
        private$den_decomposition <- matrix(1, ncol = 2)

      } else {

        D <- fuse(private$num_decomposition,
                  private$den_decomposition,
                  `-`)
        c(num, den) %<-% split_decomp(D)
        private$num_decomposition <- unique(num)
        private$den_decomposition <- unique(den)

      }


      return(invisible(self))

    },

    inverse = function() {

      if (private$is_zero) {

        stop("Cannot compute the inverse of 0.",
             call. = FALSE)

      }

      den <- unique(private$den_decomposition)
      num <- unique(private$num_decomposition)
      idx <- which(num[, 1] == -1)

      if (length(idx) > 0) {

        den <- unique(rbind(c(-1, 1),
                            den))
        num <- num[-idx, ]

      }

      den <- matrix(den, ncol = 2)
      num <- matrix(num, ncol = 2)

      private$num_decomposition <- den
      private$den_decomposition <- num

      return(invisible(self))

    },

    add = function(N) {

      if (is.numeric(N)) {

        N <- Number$new(N)

      }

      if (private$is_zero) {

        L <- N$decomposition()
        private$num_decomposition <- L$num
        private$den_decomposition <- L$den
        private$this_value <- N$value()
        private$is_zero <- N$value() == 0

        return(invisible(self))

      }

      if (N$value() == 0) {

        return(invisible(self))

      }

      L <- N$decomposition()
      other_num <- L$num
      other_den <- L$den

      this_num <- private$num_decomposition
      this_den <- private$den_decomposition

      # Common Denominator
      new_den <- fuse(this_den, other_den, op = max)

      # This numerator
      new_num1 <- fuse(fuse(new_den, this_den, `-`),
                       this_num, `+`)

      # Other numerator
      new_num2 <- fuse(fuse(new_den, other_den, `-`),
                       other_num, `+`)

      # Common part of numerators
      com_num <- fuse(new_num1, new_num2, min)

      # Uncommon this:
      uc1 <- unique(rbind(c(1, 1), fuse(new_num1, com_num, `-`)))

      # Uncommon other:
      uc2 <- unique(rbind(c(1, 1), fuse(new_num2, com_num, `-`)))

      addition <- prod(uc1[, 1] ^ uc1[, 2]) + prod(uc2[, 1] ^ uc2[, 2])

      new_fact <- unique(rbind(c(sign(addition), 1),
                               sfsmisc::factorize(abs(addition))[[1]]))

      # Final numerator:
      fin_num <- fuse(com_num, new_fact, `+`)

      private$den_decomposition <- new_den
      private$num_decomposition <- fin_num
      private$this_value <- decomp2number(fin_num) / decomp2number(new_den)

      private$is_zero <- private$this_value == 0

      return(invisible(self))

    },

    prod = function(a) {

      if (private$is_zero) return(invisible(self))

      if (is.numeric(a)) {

        a <- Number$new(a)

      }

      if (inherits(a, "Number")) {

        c(num, den) %<-% a$decomposition()
        my_den_decomp <- private$den_decomposition
        my_num_decomp <- private$num_decomposition

        new_den <- fuse(den, my_den_decomp, `+`)
        new_num <- fuse(num, my_num_decomp, `+`)

        private$num_decomposition <- unique(new_num)
        private$den_decomposition <- unique(new_den)

      }

      return(invisible(self))

    },

    power = function(n,  copy = FALSE) {

      new_num_decomposition <- private$num_decomposition
      new_num_decomposition[, 2] <- private$num_decomposition[, 2] * n
      new_den_decomposition <- private$den_decomposition
      new_den_decomposition[, 2] <- private$den_decomposition[, 2] * n

      new_this_value <- decomp2number(new_num_decomposition) / decomp2number(new_den_decomposition)

      if (copy) {

        return(Number$new(new_this_value))

      } else {

        private$num_decomposition <- new_num_decomposition
        private$den_decomposition <- new_den_decomposition
        private$this_value <- new_this_value

        return(invisible(self))

      }

    },

    sqroot = function() {

      private$num_decomposition[, 2] <- private$num_decomposition[, 2] / 2

      private$den_decomposition[, 2] <- private$den_decomposition[, 2] / 2

      if (private$this_value < 0) {

        private$this_value <- sqrt(as.complex(private$this_value))

      } else {

        private$this_value <- sqrt(private$this_value)

      }


    },

    rationalize = function() {

      den <- private$den_decomposition
      bases <- den[, 1]
      powers <- den[, 2]
      powers_int <- floor(powers)
      powers_rat <- powers - powers_int
      idx <- which(powers_rat > 0)

      if (length(idx) > 0) {

        bases_rat <- bases[idx]
        powers_rat <- 1 - powers_rat[idx]

        coef_num <- matrix(c(bases_rat, powers_rat),
                           ncol = 2)
        powers_int[idx] <- powers_int[idx] + 1
        new_den <- matrix(c(bases, powers_int),
                          ncol = 2)

        private$den_decomposition <- new_den
        private$num_decomposition <- fuse(private$num_decomposition,
                                          coef_num, op = `+`)

      }

      return(invisible(self))


    },

    decomposition = function() {

      return(list(num = private$num_decomposition,
                  den = private$den_decomposition))

    },

    print = function() {

      if (private$is_zero) {

        cat("0")

        return(invisible(NULL))

      }

      num <- private$decomp2str(private$num_decomposition)
      den <- private$decomp2str(private$den_decomposition)

      if (den != "1") {

        N <- max(c(str_length(num), str_length(den)))
        num <- str_pad(num, width = N, side = "both")
        den <- str_pad(den, width = N, side = "both")
        line <- str_flatten(rep("-", N))

        # den <- paste0(" / (", den, ")")
        den <- paste0("\n", line, "\n", den)

      } else {

        den <- ""

      }

      cat(glue::glue("{num}{den}"))

    },

    to_latex = function() {

      if (private$is_zero) {

        return("0")

      }

      num <- private$decomp2str(private$num_decomposition,
                                latex = TRUE)

      den <- private$decomp2str(private$den_decomposition,
                                latex = TRUE)

      if (den != "1") {

        str <- glue::glue("\\frac{#num#}{#den#}",
                          .open = "#", .close = "#")

      } else {

        str <- num

      }

      return(str)

    }

  ),

  private = list(

    this_value = NA,
    num_decomposition = NA,
    den_decomposition = NA,
    num_irrational = NA,
    den_irrational = NA,
    is_zero = FALSE,

    decomp2str = function(D, latex = FALSE) {

      sqrt_str <- "sqrt"
      prod_str <- "·"

      if (latex) {

        sqrt_str <- "\\sqrt"
        prod_str <- "\\cdot"

      }

      powers <- D[, 2]
      powers_int <- floor(powers)
      powers_rem <- powers - powers_int
      bases <- D[, 1]
      p_num <- numerators(powers_rem)
      p_den <- denominators(powers_rem)

      my_int <- prod(bases ^ powers_int)

      idx <- which(powers_rem == 0)
      if (length(idx) > 0) {

        p_num <- p_num[-idx]
        p_den <- p_den[-idx]
        bases <- bases[-idx]

      }

      if (length(p_num) > 0) {

        roots <- p_den %>% unique() %>% sort()

        my_root <- c()

        for (r in roots) {

          idx <- which(p_den == r)

          these_powers <- p_num[idx]
          these_bases <- bases[idx]
          this_value <- prod(these_bases ^ these_powers)

          if (this_value != 1) {

            if (r != 2) {

              my_root <- c(my_root,
                           glue::glue("#sqrt_str#[#r#]{#this_value#}", .open = "#", .close = "#"))
            } else {

              my_root <- c(my_root,
                           glue::glue("#sqrt_str#{#this_value#}",
                                      .open = "#",
                                      .close = "#"))

            }

          }

        }

        my_root <- str_flatten(my_root, prod_str)

      } else {

        my_root <- ""

      }

      my_int_str <- ifelse(abs(my_int) == 1,
                           ifelse(my_int == 1, "", "-"),
                           as.character(my_int))

      op <- ifelse((abs(my_int) != 1) && str_length(my_root) > 0,
                   prod_str, "")
      number <- glue::glue("{my_int_str}{op}{my_root}")
      if (str_length(number) == 0) {

        number <- "1"

      }

      if (number == "-") {

        number <- "-1"

      }

      return(number)

    }

  )

)

#' @export
Vector <- R6::R6Class(

  classname = "Vector",

  public = list(

    initialize = function(v) {

      if (inherits(v, "Vector")) {

        # print("Vector")

        for (i in seq(v$dim())) {

          private$components[[i]] <- Number$new(v$get_i(i))

        }


      } else {

        if (is.list(v)) {

          for (i in seq_along(v)) {

            private$components[[i]] <- Number$new(v[[i]])

          }

        } else {

          for (i in seq_along(v)) {

            private$components[[i]] <- Number$new(v[i])

          }

        }

      }

    },

    get_i = function(i) {

      if (i > self$dim()) {

        stop("Error: not so many elements.",
             call. = FALSE)

      }
      return(private$components[[i]])

    },

    dim = function() {

      length(private$components)

    },

    norm = function() {

      # val <- Number$new(0)
      val <- 0
      for (i in seq(self$dim())) {

        # val$add(private$components[[i]]$power(2))
        vi2 <- private$components[[i]]$power(2, copy = TRUE)
        val <- val + vi2$value()

      }

      val <- Number$new(val)

      val$sqroot()

      return(val)

    },

    simplify = function() {

      for (i in seq(self$dim())) {

        private$components[[i]] <- private$components[[i]]$simplify()

      }

      return(invisible(self))

    },

    rationalize = function() {

      for (i in seq(self$dim())) {

        private$components[[i]] <- private$components[[i]]$rationalize()

      }

      return(invisible(self))

    },

    sum = function(a) {

      if (inherits(a, "Vector")) {

        val <- 0

        for (i in seq(self$dim())) {

          vi <- private$components[[i]]$clone(deep = TRUE)
          val_a <- a$get_i(i)
          val <- val_a$value() + vi$value()
          private$components[[i]] <- Number$new(val)

        }

      }

      return(invisible(self))

    },

    prod = function(a) {

      if (inherits(a, "Number") || is.numeric(a)) {

        for (i in seq(self$dim())) {

          private$components[[i]]$prod(a)

        }

      }

      if (inherits(a, "Vector")) {

        val <- Number$new(0)

        for (i in seq(self$dim())) {

          vi <- Number$new(self$get_i(i))
          second <- Number$new(a$get_i(i))
          vi$prod(second)
          vi$simplify()
          vi$rationalize()
          val$add(vi)

        }

        return(val)

      }

      return(invisible(self))

    },

    print = function() {

      for (i in seq(self$dim())) {

        cat(private$components[[i]]$print(), "\n")
        cat("\n")

      }

    },

    to_latex = function(ldeco = "\\left(",
                        rdeco = "\\right)") {

      str <- c(ldeco, "\\begin{array}{c}",
               sapply(private$components, function(r) r$to_latex()) %>% str_flatten("\\\\\n"),
               "\\end{array}", rdeco) %>% str_flatten("\n")

      return(str)

    }

  ),

  private = list(

    components = list()

  )

)

find_power <- function(D, factor) {

  idx <- which(D[, 1] == factor)
  if (length(idx) == 0) {

    return(0)

  } else {

    return(D[idx, 2])

  }

}


fuse <- function(D1, D2, op) {


  bases <- sort(unique(c(D1[, 1], D2[, 1])))
  new_decomp <- matrix(NA, ncol = 2, nrow = length(bases))

  for (b_idx in seq_along(bases)) {

    b <- bases[b_idx]

    p1 <- find_power(D1, b)
    p2 <- find_power(D2, b)

    new_decomp[b_idx, 1] <- b
    new_decomp[b_idx, 2] <- op(p1, p2)

  }

  idx0 <- which(new_decomp[, 2] == 0)
  if (length(idx0) > 0) {

    new_decomp <- new_decomp[-idx0, ]

  }

  new_decomp <- matrix(new_decomp, ncol = 2)

  return(new_decomp)

}

decomp2number <- function(D) {

  bases <- D[, 1]
  powers <- D[, 2]
  return(prod(bases ^ powers))

}

split_decomp <- function(D) {

  idx <- which(D[, 2] < 0)

  if (length(idx) > 0) {

    num <- matrix(D[-idx, ], ncol = 2)
    den <- matrix(D[idx, ], ncol = 2)
    den[, 2] <- -den[, 2] # positive powers

  } else {

    num <- matrix(D, ncol = 2)
    den <- matrix(1, ncol = 2, nrow = 1)

  }

  return(list(num = num, den = den))

}

common_factors <- function(...) {

  dots <- list(...)

  fmin <- function(a, b) fuse(a, b, op = min)
  fmax <- function(a, b) fuse(a, b, op = max)

  init <- Reduce(f = fmax,
                 x = dots,
                 init = matrix(1, ncol = 2))

  Reduce(f = fmin, x = dots, init = init)

}

dot_product <- function(v1, v2) {

  v <- v1$clone(deep = TRUE)
  v$prod(v2)

  product <- 0
  for (i in seq(v$dim())) {

    vi <- v$get_i(i)
    product <- product + vi$value()

  }

  return(product)

}

#' @export
vectors2matrix <- function(B) {

  if (!is.list(B)) {

    stop("Argument must be a list of 'Vector's.",
         call. = FALSE)

  }

  if (!inherits(B[[1]], "Vector")) {

    stop("Argument must be a list of 'Vector's.",
         call. = FALSE)

  }

  m <- length(B)
  n <- B[[1]]$dim()

  A <- matrix("", ncol = m, nrow = n)

  for (i in seq(n)) {

    for (j in seq(m)) {

      number <- B[[j]]$get_i(i)
      A[i, j] <- number$to_latex()

    }

  }

  return(A)

}