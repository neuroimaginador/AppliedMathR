rlatex <- function(options) {

  if (options$eval) {

    the_code <- paste0("'",
                       xfun::split_lines(options$code),
                       "', ") %>%
      stringr::str_replace_all(stringr::fixed("\\"),
                               "\\\\")

    the_code <- c("glue::glue(", the_code,
                  ", .open = '[', .close = ']', .sep = ' ')") %>%
      stringr::str_flatten(" ")

    out <- eval(parse(text = the_code)) %>%
      stringr::str_flatten(" ")

    out <- c("\\[", out, "\\]") %>%
      stringr::str_flatten(" ")

    options$echo <- FALSE
    options$results <- "asis"
    knitr::engine_output(options,
                         options$code,
                         out)

  }

}

rtikz <- function(options) {

  if (options$eval) {

    the_code <- options$code

    if (!dir.exists(options$fig.path)) {

      dir.create(options$fig.path,
                 showWarnings = FALSE,
                 recursive = TRUE)

    }

    filename <- file.path(options$fig.path,
                          paste0(options$label, ".tex"))

    tikzDevice::tikz(filename,
                     width = options$fig.width,
                     height = options$fig.height)
    print(eval(parse(text = the_code)))
    invisible(grDevices::dev.off())

    out <- paste0("\\input{",
                  filename,
                  "}")

    options$echo <- FALSE
    options$results <- "asis"
    knitr::engine_output(options,
                         options$code,
                         out)

  }

}

