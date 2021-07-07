library(rlang)
library(purrr)

#' @export
to_math <- function(x, env = parent.frame()) {

  expr <- rlang::enexpr(x)
  # print(expr)
  out <- rlang::eval_bare(expr,
                          rlang::env_clone(env, latex_env(expr)))
  # out <- eval_bare(expr, latex_env(expr))
  # print(out)
  latex(out)

}

latex <- function(x) structure(x, class = "advr_latex")
print.advr_latex <- function(x) {
  cat("<LATEX> ", x, "\n", sep = "")
}

greek <- c(
  "alpha", "theta", "tau", "beta", "vartheta", "pi", "upsilon",
  "gamma", "varpi", "phi", "delta", "kappa", "rho",
  "varphi", "epsilon", "lambda", "varrho", "chi", "varepsilon",
  "mu", "sigma", "psi", "zeta", "nu", "varsigma", "omega", "eta",
  "xi", "Gamma", "Lambda", "Sigma", "Psi", "Delta", "Xi",
  "Upsilon", "Omega", "Theta", "Pi", "Phi"
)
greek_list <- rlang::set_names(paste0("\\", greek), greek)
greek_env <- rlang::as_environment(greek_list)

latex_env <- function(expr) {
  greek_env
}

expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

switch_expr <- function(x, ...) {
  switch(expr_type(x),
         ...,
         stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}

recurse_call <- function(x) {
  switch_expr(x,
              # Base cases
              symbol = ,
              constant = ,

              # Recursive cases
              call = ,
              pairlist =
  )
}

flat_map_chr <- function(.x, .f, ...) {
  purrr::flatten_chr(purrr::map(.x, .f, ...))
}

all_names_rec <- function(x) {
  switch_expr(x,
              constant = character(),
              symbol =   as.character(x),
              call =     flat_map_chr(as.list(x[-1]), all_names)
  )
}

all_names <- function(x) {
  unique(all_names_rec(x))
}

latex_env <- function(expr) {
  # Unknown symbols
  names <- all_names(expr)
  symbol_env <- as_environment(set_names(names))

  # Known symbols
  env_clone(greek_env, parent = symbol_env)
}

unary_op <- function(left, right) {
  rlang::new_function(
    rlang::exprs(e1 = ),
    rlang::expr(
      paste0(!!left, e1, !!right)
    ),
    rlang::caller_env()
  )
}

as_is_op <- function(op) {

  f <- function(e1, e2) {

    if (!missing(e1)) {

      if (!missing(e2)) {

        paste0(e1, op, e2)

      } else {

        # paste0(op, "\\left(", e1, "\\right)")
        paste0(op, e1)

      }

    } else {

      if (!missing(e2)) {

        paste0(op, e2)

      } else {

        paste(op)

      }

    }

  }

  parent.env(environment(f)) <- rlang::caller_env()

  return(f)

}

binary_op <- function(sep) {
  rlang::new_function(
    rlang::exprs(e1 = , e2 = ),
    rlang::expr(
      paste0("{", e1, "}",
             !!sep,
             "{", e2, "}")
    ),
    rlang::caller_env()
  )
}

# Binary operators
f_env <- rlang::child_env(
  .parent = rlang::empty_env(),
  `+` = binary_op(" + "),
  `-` = as_is_op(" - "),
  # `*` = binary_op(" \\cdot "),
  `*` = binary_op(" "),
  `/` = binary_op(" / "),
  `^` = binary_op("^"),
  `[` = binary_op("_"),

  # Grouping
  `{` = unary_op("\\left{ ", " \\right}"),
  `(` = unary_op("\\left( ", " \\right)"),
  paste = paste,

  # Negative
  # `-` = unary_op("- ", ""),

  # Other math functions
  sqrt = unary_op("\\sqrt{", "}"),
  exp = unary_op("\\mathrm{e}^{", "}"),
  sin =  unary_op("\\sin(", ")"),
  cos =  unary_op("\\cos(", ")"),
  log =  unary_op("\\log(", ")"),
  abs =  unary_op("\\left| ", "\\right| "),
  frac = function(a, b) {
    paste0("\\frac{", a, "}{", b, "}")
  },

  # Labelling
  hat =   unary_op("\\hat{", "}"),
  tilde = unary_op("\\tilde{", "}")
)

latex_env <- function(expr) {
  # Known functions
  f_env

  # Default symbols
  names <- all_names(expr)
  symbol_env <- rlang::as_environment(rlang::set_names(names),
                                      parent = f_env)

  # Known symbols
  greek_env <- rlang::env_clone(greek_env,
                                parent = symbol_env)

  greek_env

}

all_calls_rec <- function(x) {
  switch_expr(x,
              constant = ,
              symbol =   character(),
              call = {
                fname <- as.character(x[[1]])
                children <- flat_map_chr(as.list(x[-1]), all_calls)
                c(fname, children)
              }
  )
}
all_calls <- function(x) {
  unique(all_calls_rec(x))
}

unknown_op <- function(op) {
  rlang::new_function(
    rlang::exprs(... = ),
    rlang::expr({
      contents <- paste(..., collapse = ", ")
      paste0(!!paste0("\\mathrm{", op, "}("), contents, ")")
    })
  )
}

latex_env <- function(expr) {

  calls <- all_calls(expr)
  call_list <- purrr::map(rlang::set_names(calls), unknown_op)
  call_env <- rlang::as_environment(call_list)

  # Known functions
  f_env <- rlang::env_clone(f_env, call_env)

  # Default symbols
  names <- all_names(expr)
  symbol_env <- rlang::as_environment(rlang::set_names(names),
                                      parent = f_env)

  # Known symbols
  greek_env <- rlang::env_clone(greek_env,
                                parent = symbol_env)
  greek_env

}

write_system
