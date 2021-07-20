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
    options( tikzLatexPackages = c(
      "\\usepackage{tikz}",
      "\\usepackage[active,tightpage,psfixbb]{preview}",
      "\\PreviewEnvironment{pgfpicture}",
      "\\setlength\\PreviewBorder{0pt}",
      # getOption( "tikzLatexPackages" ),
      "\\usepackage{amssymb}"
    ))

    tikzDevice::tikz(filename,
                     standAlone = knitr::is_html_output(),
                     width = options$fig.width,
                     height = options$fig.height)
    print(eval(parse(text = the_code)))
    invisible(grDevices::dev.off())

    if (knitr::is_html_output()) {

      outfile <- tinytex::pdflatex(filename)

      # print(filename)
      #
      # outf <- tinytex::latexmk(filename,
      #                          "latex")
      #
      # fig = knitr::fig_path(".dvi", options)
      # dir.create(dirname(fig), recursive = TRUE, showWarnings = FALSE)
      # file.rename(outf, fig)
      # fig2 <- xfun::with_ext(fig, ".svg")
      # # if (to_svg) {
      # if (Sys.which("dvisvgm") == "")
      #   tinytex::tlmgr_install("dvisvgm")
      # if (system2("dvisvgm", c("-o", shQuote(fig2), fig)) !=
      #     0)
      #   stop("Failed to compile ", fig, " to ", fig2)
      # # }
      # fig <- fig2
      # options$fig.num <- 1L
      # options$fig.cur <- 1L
      # extra = knitr:::run_hook_plot(fig,
      #                              options)
      # options$engine <- "tex"
      # knitr::engine_output(options, options$code, "", extra)

      bitmap <- pdftools::pdf_render_page(
        outfile,
        page = 1,
        dpi = 300)
      png::writePNG(
        bitmap,
        paste0(outfile, ".png"))

      outfile <- paste0(outfile, ".png")
      out <- paste0("![](", outfile, ")")
      options$echo <- FALSE
      options$results <- "asis"
      knitr::engine_output(options,
                           options$code,
                           out)

    } else {

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

}

