
# functions which mask internal / base R equivalents, usually to provide
# backwards compatibility or guard against common errors

numeric_version <- function(x, strict = TRUE) {
  base::numeric_version(as.character(x), strict = strict)
}

sprintf <- function(fmt, ...) {
  message <- if (nargs() == 1L) fmt else base::sprintf(fmt, ...)
  ansify(message)
}

substring <- function(text, first, last = .Machine$integer.max) {

  n <- length(text)
  if (n == 0L)
    return(text)

  m <- max(n, length(first), length(last))
  text <- rep_len(as.character(text), length.out = m)
  substr(text, first, last)

}

unique <- function(x) {
  base::unique(x)
}

# when renv is embedded, renv itself might not be installed; vendor() copies the
# resources renv reads at runtime into the host package's 'inst/vendor'
# directory, so prefer those when looking up renv's own files
system.file <- function(..., package = "base", lib.loc = NULL, mustWork = FALSE) {

  # find the next system.file() up the chain, rather than calling
  # base::system.file() directly, so that pkgload's shim is used under
  # devtools::load_all()
  impl <- get("system.file", envir = parent.env(renv_envir_self()))

  # fall through to the regular lookup for files vendor() doesn't bundle,
  # so that those still resolve against an installed copy of renv
  if (identical(package, "renv") && isTRUE(renv_metadata_embedded())) {
    path <- impl("vendor", ..., package = .packageName, lib.loc = lib.loc)
    if (nzchar(path))
      return(path)
  }

  impl(..., package = package, lib.loc = lib.loc, mustWork = mustWork)

}

# a wrapper for 'utils::untar()' that throws an error if untar fails
untar <- function(tarfile,
                  files = NULL,
                  list = FALSE,
                  exdir = ".",
                  tar = Sys.getenv("TAR"))
{
  # delegate to utils::untar()
  result <- utils::untar(
    tarfile = tarfile,
    files   = files,
    list    = list,
    exdir   = exdir,
    tar     = tar
  )

  # check for errors (tar returns a status code)
  if (is.integer(result) && result != 0L) {
    call <- stringify(sys.call())
    stopf("'%s' returned status code %i", call, result)
  }

  # return other results as-is
  result
}

# prefer writing files as UTF-8
writeLines <- function(text, con = stdout(), sep = "\n", useBytes = FALSE) {
  if (is.character(con) && missing(useBytes))
    base::writeLines(enc2utf8(text), con = con, sep = sep, useBytes = TRUE)
  else
    base::writeLines(text, con, sep, useBytes)
}
