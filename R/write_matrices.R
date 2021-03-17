#' @export
glue_matrices <- function(...,
                           latex = FALSE,
                           fractions = TRUE,
                           format = "c",
                           sep = "|",
                           ldeco = ifelse(latex, "\\left(", ""),
                           rdeco = ifelse(latex, "\\right)", "")) {

  matrices <- list(...)

  if ((length(matrices) == 1) && (is.list(matrices[[1]]))) {

    args <- append(matrices[[1]],
                   list(latex = latex,
                        fractions = fractions,
                        format = format,
                        sep = sep,
                        ldeco = ldeco,
                        rdeco = rdeco))

    return(do.call(glue_matrices, args))

  }

  # Basic checks
  ncols <- sapply(matrices, ncol)
  nrows <- sapply(matrices, nrow)

  if (length(unique(nrows)) > 1) {

    stop("Not all the matrices have the same number of rows.",
         call. = FALSE)

  }

  if (fractions) {

    m <- lapply(matrices, function(A) {

      return(to_fraction(A, latex = latex))

    })

    matrices <- m

  }

  M <- do.call(cbind, matrices)

  # Parse separator
  if ((length(sep) != 1) &&
      (length(sep) != length(matrices) - 1)) {

    stop("Incorrect format for 'sep'.",
         call. = FALSE)

  }

  if ((length(sep) == 1) && (str_length(sep) > 0)) {

    sep <- rep(sep, length(matrices) - 1)

  }

  if (!latex) {

    sep <- paste0(" ", sep, " ")

  } else {

    sep <- paste0("@{", sep, "}")

  }

  sep[sep == "@{|}"] <- "|"
  sep[sep = "@{}"] <- ""

  # Column width
  col_width <- apply(M, 2, function(m) max(str_length(m))) + 2

  # Some matrices can be collapsed into one column
  collapse <- rep(FALSE, length(matrices))

  # Alignment of each column. Parse argument "format"
  # Must have one of:
  # length 1 -> same format for all columns of all matrices
  # length == length(matrices) -> a different format for each matrix
  # length == ncol(M) -> a format for each column
  if (length(format) == 1) {

    if (str_length(format) == 1) {

      alignment <- lapply(seq_along(matrices),
                          function(i)
                            rep(format, ncols[i]))

    } else {

      alignment <- as.list(rep(format, length(matrices)))
      format <- alignment

    }

  }
  if (length(format) == length(matrices)) {
    # A different format for each matrix
    # format[i] can be:
    # length 1 and in c("c", "l", "r") -> same format for all columns
    # length 1 and in c("C", "L", "R") -> collapse all columns and apply formatting
    # length == ncol -> a format for each column

    alignment <- list()

    for (i in seq_along(matrices)) {

      fi <- format[i]

      if (str_length(fi) == ncols[i]) {

        alignment[[i]] <- str_split(fi, "")[[1]]
        next

      }
      if (str_length(fi) == 1) {

        if (fi %in% c("c", "l", "r")) {

          alignment[[i]] <- rep(fi, ncols[i])

        }
        if (fi %in% c("C", "L", "R")) {

          alignment[[i]] <- tolower(fi)
          collapse[i] <- TRUE
          ncols[i] <- 1

        }

        next

      }

    }

  }
  if (length(format) == ncol(M)) {

    alignment <- lapply(seq_along(matrices),
                        function(i)
                          rep(format[i], ncols[i])
    )

  }

  align_sep <- alignment %>%
    lapply(str_flatten) %>%
    str_alternate(sep) %>%
    str_flatten()

  alignment <- unlist(alignment)

  # String format for writing a single row
  if (!latex) {

    fmt_row <- sapply(
      seq_along(ncols),
      function(m) {

        str_flatten(rep("%s ", ncols[m]))

      }) %>% str_alternate(sep) %>% str_flatten()

  } else {

    fmt_row <- rep("%s", sum(ncols)) %>% str_flatten(" & ")

  }

  # Process rows to obtain a vector of characters for each one
  if (!latex) {

    alignment[alignment == "r"] <- "right"
    alignment[alignment == "l"] <- "left"
    alignment[alignment == "c"] <- "both"

    M_str <- list()

    for (i in seq(nrow(M))) {

      M_str[[i]] <- sapply(
        seq(sum(ncols)),
        function(j) {

          str_pad(M[i, j],
                  width = col_width[j],
                  side = alignment[j])

        })

    }

  } else {

    M_str <- lapply(seq(nrow(M)),
                    function(i) as.character(M[i, ]))

  }

  # Body
  write_row <- function(...) do.call(pryr::partial(sprintf,
                                                   fmt = fmt_row),
                                     as.list(...))
  body <- sapply(M_str, write_row)
  if (!latex) {

    body <- body %>% str_flatten("\n")

  } else {

    body <- body %>% str_flatten("\\\\\n")

  }

  # Header (includes left and right decoration)
  if (latex) {

    if (str_length(ldeco) > 0) {

      ldeco <- str_flatten(c(ldeco, "\n"))

    }

    if (str_length(rdeco) > 0) {

      rdeco <- str_flatten(c(rdeco, "\n"))

    }

    header <- paste0(#"\\ensuremath{\n",
                     ldeco,
                     "\\begin{array}{",
                     str_flatten(align_sep), "}",
                     "\n",
                     collapse = "")

    footer <- paste0("\n\\end{array}\n",
                     rdeco,
                     #"}",
                     "\n")

  } else {

    header <- ldeco
    footer <- rdeco

  }

  output <- paste0(header, body, footer)
  output <- output %>%
    str_replace_all(pattern = "\n+",
                    replacement = "\n")

  return(output)

}

str_alternate <- function(str1, str2) {

  # stopifnot(length(str1) >= length(str2))
  idx <- c(seq_along(str1), seq_along(str2))
  o <- order(idx)
  str <- c(str1, str2)[o]

  return(str)

}

#' @export
glue_latex <- function(...) {

  c("\\[",
    glue::glue(...,
             .open = "[", .close = "]",
             .sep = ""),
    "\\]")

}

format_row <- function(row, alignment, collapse = FALSE) {




}
