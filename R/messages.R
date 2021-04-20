divide_msg <- function() {

  locale <- Sys.getlocale("LC_MESSAGES")
  if (stringr::str_detect(locale, "es_ES")) {

    "dividir"

  } else {

    "divide"

  }

}

multiply_msg <- function() {

  locale <- Sys.getlocale("LC_MESSAGES")
  if (stringr::str_detect(locale, "es_ES")) {

    "multiplicar"

  } else {

    "multiply"

  }

}

permute_msg <- function() {

  locale <- Sys.getlocale("LC_MESSAGES")
  if (stringr::str_detect(locale, "es_ES")) {

    "intercambiar"

  } else {

    "permute"

  }

}

by_msg <- function() {

  locale <- Sys.getlocale("LC_MESSAGES")
  if (stringr::str_detect(locale, "es_ES")) {

    "por"

  } else {

    "by"

  }

}

and_msg <- function() {

  locale <- Sys.getlocale("LC_MESSAGES")
  if (stringr::str_detect(locale, "es_ES")) {

    "y"

  } else {

    "and"

  }

}

columns_msg <- function(plural = TRUE) {

  locale <- Sys.getlocale("LC_MESSAGES")
  if (stringr::str_detect(locale, "es_ES")) {

    if (plural) "columnas" else "columna"

  } else {

    if (plural) "columns" else "column"

  }

}

rows_msg <- function(plural = TRUE) {

  locale <- Sys.getlocale("LC_MESSAGES")
  if (stringr::str_detect(locale, "es_ES")) {

    if (plural) "filas" else "fila"

  } else {

    if (plural) "rows" else "row"

  }

}

add_msg <- function() {

  locale <- Sys.getlocale("LC_MESSAGES")
  if (stringr::str_detect(locale, "es_ES")) {

    "sumar"

  } else {

    "add"

  }

}

to_msg <- function() {

  locale <- Sys.getlocale("LC_MESSAGES")
  if (stringr::str_detect(locale, "es_ES")) {

    "a"

  } else {

    "to"

  }

}