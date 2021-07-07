#' @importFrom grDevices dev.off
.onAttach <- function(libname, pkgname) {

  knitr::knit_engines$set(
    rlatex = rlatex,
    rtikz = rtikz,
    theorem = theorem_engine,
    definition = definition_engine)

}
