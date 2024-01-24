tikz_ticks <- function(xi) {

  str <- "\\draw[dashed,-] ({xi}, -0.1) -- ({xi},0.1) node[below] {{\\footnotesize ${xi}$}}"

  str %>% glue::glue(xi = xi)
}

tikz_axis <- function(xlim, ylim) {

  str <- "\\draw[->] ({xlim[1]} ,0) -- ({xlim[2]}, 0) node[right] {{$x$}};\n\\draw[->] (0, {ylim[1]}) -- (0, {ylim[2]}) node[above] {{$y$}};"

  str %>% glue::glue(xlim  = xlim, ylim = ylim)
}

tikz_f <- function(f, interval) {

  xlim <- interval

  fstr <- f %>% stringr::str_replace_all("x", "\\\\x")
  str <- "\\draw[domain={xlim[1]}:{xlim[2]},smooth,variable=\\x,color = blue,thick] plot ({{\\x}},{{{fstr}}});"

  str %>% glue::glue(xlim = xlim, fstr = fstr)

}

tikz_path <- function(xx, yy) {

  str <- "\\path[line width= 0.6pt,line join=round]"

  str2 <- paste0("(", xx, ", ", yy, ")") %>%
    stringr::str_flatten(" -- \n")

  paste0(str, " ", str2, ";")

}

#' @export
tikz_plotf <- function(f, interval, npoints = 100) {

  xlim <- interval
  xx <- seq(xlim[1], xlim[2], length.out = npoints) %>% round(3)
  yy <- eval(str2expression(f), envir = list(x = xx)) %>% round(3)
  ylim <- c(floor(min(yy)), ceiling(max(yy)))

  axes <- tikz_axis(xlim, ylim)
  ticks <- tikz_ticks(xi = seq(xlim[1], xlim[2]))
  fun <- tikz_path(xx, yy)

  c("\\begin{tikzpicture}",
    "",
    axes,
    ticks,
    "",
    fun,
    "",
    "\\end{tikzpicture}") %>% stringr::str_flatten("\n")

}
