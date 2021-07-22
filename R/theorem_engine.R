theorem_engine <- function(options) {

  desc <- options$name
  text <- options$code

  if (knitr::is_html_output()) {

    if (stringr::str_length(desc) > 0) {

      # desc <- paste0("\n> __", desc, "__:\n")
      attribution <- paste0(" data-attribution = '",
                            desc, "' ")

    } else {

      attribution <- ""

    }

    text <- stringr::str_split(text, "\n") %>%
      unlist() %>%
      stringr::str_flatten("\n")

    # text <- stringr::str_split(text, "\n")[[1]]
    # text <- glue::glue("\n> {text}") %>%
    #   stringr::str_flatten("\n")

    out <- glue::glue("\n<div class = 'theor'{attribution}>\n {text}\n</div>") %>%
      stringr::str_flatten("\n")


    # out <- c(paste0(desc, " ", text), "\n") %>%
    #   stringr::str_flatten("\n")

    # cat(desc, text, "\n\n")

  } else {

    if (stringr::str_length(desc) > 0) {

      desc <- paste0("[", desc, "]")

    }

    out <- glue::glue(
      "\\vspace*{2mm}\n\\begin{theo}#desc#\n",
      "#md2latex(text)#\n\\end{theo}\n\n",
      .open = "#", .close = "#"
    )

  }

  options$echo <- FALSE
  options$results <- "asis"
  knitr::engine_output(options,
                       options$code,
                       out)

}

definition_engine <- function(options) {

  desc <- options$name
  text <- options$code
  # message(text)

  if (knitr::is_html_output()) {

    if (stringr::str_length(desc) > 0) {

      # desc <- paste0("\n> __", desc, "__:\n")
      entity <- paste0(" entity = '",
                       desc, "' ")

    } else {

      entity <- ""

    }

    text <- stringr::str_split(text, "\n") %>%
      unlist() %>%
      stringr::str_flatten("\n")
    message(text)
    # text <- glue::glue("\n> {text}") %>%
    #   stringr::str_flatten("\n")
    # out <- c(paste0(desc, " ", text), "\n") %>%
    #   stringr::str_flatten("\n")

    out <- glue::glue("\n<div class = 'defin'{entity}>\n {text}\n</div>") %>%
      stringr::str_flatten("\n")



    # cat(desc, text, "\n\n")

  } else {

    if (stringr::str_length(desc) > 0) {

      desc <- paste0("[", desc, "]")

    }

    out <- glue::glue(
      "\\vspace*{2mm}\n\\begin{defin}#desc#\n",
      "#md2latex(text)#\n\\end{defin}\n\n",
      .open = "#", .close = "#"
    )

  }

  options$echo <- FALSE
  options$results <- "asis"
  options$engine <- "markdown"

  knitr::engine_output(options,
                       options$code,
                       out)

}

md2latex <- function(text) {

  filename <- pander::Pandoc.convert(text = text,
                                     format = "latex",
                                     open = FALSE)

  latex <- readr::read_lines(filename)

  idx <- match(c("\\begin{document}", "\\end{document}"), latex)

  if (length(idx) == 2) {

    return(str_flatten(latex[(idx[1] + 1):(idx[2] - 1)], " "))

  }

}

#' @export
begin_example <- function(desc = "") {

  if (knitr::is_html_output()) {

    if (stringr::str_length(desc) > 0) {

      entity <- paste0(" entity = '",
                       desc, "' ")

    } else {

      entity <- ""

    }

    out <- glue::glue("\n<div class = 'examp'{entity}>\n") %>%
      stringr::str_flatten("\n")

  } else {

    if (stringr::str_length(desc) > 0) {

      desc <- paste0("[", desc, "]")

    }

    out <- glue::glue(
      "\\vspace*{2mm}\n\\begin{examp}#desc#\n",
      .open = "#", .close = "#"
    )

  }

  # cat(out)

  return(invisible(out))

}

#' @export
end_example <- function() {

  if (knitr::is_html_output()) {

    out <- "\n</div>\n"

  } else {

    out <- "\n\\end{examp}\n\n"

  }

  # cat(out)
  return(invisible(out))

}
