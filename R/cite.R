
# TODO
#
# Different citation styles:
# - https://pandoc.org/MANUAL.html#citations
# - https://www.zotero.org/styles
# - https://pandoc.org/MANUAL.html#option--csl
#
# Path to pandoc? Safe pandoc invocation?
# Output formats? (text, markdown, etc)
# Write to file? (e.g. file called PACKAGE-CITATIONS?)
# Include only top-level packages? All recursive dependencies?
# Allow package-defined citations, or form only from DESCRIPTION?
# What about packages with multiple bibtex entries?
cite <- function(type = c("plain", "bibtex"),
                 ...,
                 project = NULL)
{
  renv_scope_error_handler()
  renv_dots_check(...)

  project <- project %||% renv_project()
  type <- match.arg(type)

  packages <- sort(.packages(all.available = TRUE))
  names(packages) <- packages
  vprintf("Generating citations ... ")
  citations <- lapply(packages, function(package) {
    catchall(citation(package))
  })
  vwritef("Done!")

  erridx <- map_lgl(citations, inherits, what = "condition")
  errs <- citations[erridx]
  if (length(errs)) {

      msg <- paste(
      format(paste0(names(errs), ":")),
      map_chr(errs, conditionMessage)
    )

    renv_pretty_print(
      msg,
      "renv could not generate citations for the following packages:",
      "You may need to manually form a citation for these packages.",
      wrap = FALSE
    )

  }

  bibtex <-
    enumerate(citations, renv_citation_bibtex) %>%
    filter(is.character) %>%
    unlist()

  for (i in seq_along(bibtex)) {
    replacement <- sprintf("{citation%i,", i)
    bibtex[[i]] <- sub("{,", replacement, bibtex[[i]], fixed = TRUE)
    bibtex[[i]] <- sub("title = \\{(.*)\\}", "title = {{\\1}}", bibtex[[i]])
  }

  dir <- tempfile("renv-cite-")
  dir.create(dir, recursive = TRUE)
  owd <- setwd(dir)
  on.exit(setwd(owd), add = TRUE)

  writeLines(paste(bibtex, collapse = "\n"), con = "bibliography.bib")

  md <- c(
    "---",
    "bibliography: bibliography.bib",
    "biblio-style: apalike",
    "nocite: '@*'",
    "---",
    "",
    "# Bibliography"
  )

  writeLines(md, con = "input.md")

  args <- c(
    "--bibliography=bibliography.bib",
    "--standalone",
    "input.md",
    "-o",
    "output.html"
  )

  system2("pandoc", args)
  system("open output.html")

}

renv_citation_bibtex <- function(package, citation) {

  status <- catch(toBibtex(citation))
  if (inherits(status, "error")) {
    fmt <- "could not generate bibtex for package '%s'"
    warningf(fmt, package)
    return(NULL)
  }

  status

}
