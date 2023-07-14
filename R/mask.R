
# functions which mask internal / base R equivalents, usually to provide
# backwards compatibility or guard against common errors

numeric_version <- function(x, strict = TRUE) {
  base::numeric_version(as.character(x), strict = strict)
}

sprintf <- function(fmt, ...) {
  if (nargs() == 1L)
    fmt
  else
    base::sprintf(fmt, ...)
}

unique <- function(x) {
  base::unique(x)
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

