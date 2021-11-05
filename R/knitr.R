
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
