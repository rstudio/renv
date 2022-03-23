
renv_knitr_options_header <- function(text, type) {

  # extract the inner options from the header
  patterns <- renv_knitr_patterns()
  rest <- sub(patterns[[type]]$chunk.begin, "\\1", text)

  # if this is an R Markdown document, parse the initial engine chunk
  # (default to 'r' when not set)
  engine <- "r"
  if (type == "md") {
    idx <- regexpr("(?:[ ,]|$)", rest)
    engine <- substring(rest, 1, idx - 1)
    rest <- sub("^,*\\s*", "", substring(rest, idx + 1))
  }

  # parse the params
  params <- renv_knitr_options_header_impl(rest)

  # ensure an engine is set, if any
  params[["engine"]] <- params[["engine"]] %||% engine

  # return parsed params
  params

}

renv_knitr_options_header_impl <- function(rest) {

  # extract an unquoted label
  label <- ""
  pattern <- "(^\\s*[^=]+)(,|\\s*$)"
  matches <- regexec(pattern, rest)[[1]]
  if (!identical(c(matches), -1L)) {
    submatches <- regmatches(rest, list(matches))[[1]]
    label <- trimws(submatches[[2L]])
    rest <- substring(rest, matches[[3L]] + 1L)
  }

  # parse as alist
  params <- catch(parse(text = sprintf("alist(%s)", rest))[[1]])
  if (inherits(params, "error"))
    return(list())

  # inject the label back in
  names(params) <- names(params) %||% rep.int("", length(params))
  if (length(params) > 1 && names(params)[[2L]] == "")
    names(params)[[2L]] <- "label"

  # fix up 'label' if it's a missing value
  if (identical(params[["label"]], quote(expr = )))
    params[["label"]] <- NULL

  # if we parsed a label, add it in
  if (is.null(params[["label"]]) && nzchar(label))
    params[["label"]] <- label

  # evaluate the alist
  eval(params, envir = parent.frame())

}

renv_knitr_options_chunk <- function(code) {

  # find chunk option lines
  pattern <- "^[[:space:]]*#+[|]"
  matches <- grep(pattern, code[nzchar(code)], value = TRUE)

  # remove prefix
  text <- gsub(pattern, "", matches)

  # try to guess whether it's YAML
  isyaml <- any(grepl("^[[:space:]]*[^[:space:]:]+:", text))

  # first, try to parse as YAML, then as R code
  params <- if (isyaml) {
    catch(renv_yaml_load(text))
  } else {
    code <- paste(text, collapse = ", ")
    catch(renv_knitr_options_header_impl(code))
  }

  # check for error and report if this is in dependency discovery
  if (inherits(params, "error")) {

    state <- renv_dependencies_state()
    if (!is.null(state)) {
      problem <- list(file = state$path %||% "<unknown>", error = params)
      state$problems$push(problem)
    }

    return(list())

  }

  # return parsed params
  params

}

renv_knitr_patterns <- function() {

  list(

    rnw = list(
      chunk.begin = "^\\s*<<(.*)>>=.*$",
      chunk.end = "^\\s*@\\s*(%+.*|)$",
      inline.code = "\\\\Sexpr\\{([^}]+)\\}",
      inline.comment = "^\\s*%.*",
      ref.chunk = "^\\s*<<(.+)>>\\s*$",
      header.begin = "(^|\n)\\s*\\\\documentclass[^}]+\\}",
      document.begin = "\\s*\\\\begin\\{document\\}"
    ),

    tex = list(
      chunk.begin = "^\\s*%+\\s*begin.rcode\\s*(.*)",
      chunk.end = "^\\s*%+\\s*end.rcode",
      chunk.code = "^\\s*%+",
      ref.chunk = "^%+\\s*<<(.+)>>\\s*$",
      inline.comment = "^\\s*%.*",
      inline.code = "\\\\rinline\\{([^}]+)\\}",
      header.begin = "(^|\n)\\s*\\\\documentclass[^}]+\\}",
      document.begin = "\\s*\\\\begin\\{document\\}"
    ),

    html = list(
      chunk.begin = "^\\s*<!--\\s*begin.rcode\\s*(.*)",
      chunk.end = "^\\s*end.rcode\\s*-->",
      ref.chunk = "^\\s*<<(.+)>>\\s*$",
      inline.code = "<!--\\s*rinline(.+?)-->",
      header.begin = "\\s*<head>"
    ),

    md = list(
      chunk.begin = "^[\t >]*```+\\s*\\{([a-zA-Z0-9_]+( *[ ,].*)?)\\}\\s*$",
      chunk.end = "^[\t >]*```+\\s*$",
      ref.chunk = "^\\s*<<(.+)>>\\s*$",
      inline.code = "(?<!(^|\n)``)`r[ #]([^`]+)\\s*`"
    ),

    rst = list(
      chunk.begin = "^\\s*[.][.]\\s+\\{r(.*)\\}\\s*$",
      chunk.end = "^\\s*[.][.]\\s+[.][.]\\s*$",
      chunk.code = "^\\s*[.][.]",
      ref.chunk = "^\\.*\\s*<<(.+)>>\\s*$",
      inline.code = ":r:`([^`]+)`"
    ),

    asciidoc = list(
      chunk.begin = "^//\\s*begin[.]rcode(.*)$",
      chunk.end = "^//\\s*end[.]rcode\\s*$",
      chunk.code = "^//+",
      ref.chunk = "^\\s*<<(.+)>>\\s*$",
      inline.code = "`r +([^`]+)\\s*`|[+]r +([^+]+)\\s*[+]",
      inline.comment = "^//.*"
    ),

    textile = list(
      chunk.begin = "^###[.]\\s+begin[.]rcode(.*)$",
      chunk.end = "^###[.]\\s+end[.]rcode\\s*$",
      ref.chunk = "^\\s*<<(.+)>>\\s*$",
      inline.code = "@r +([^@]+)\\s*@",
      inline.comment = "^###[.].*"
    )

  )

}
