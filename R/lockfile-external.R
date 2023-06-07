renv_lockfile_external <- function() {
  list(
    quarto = quarto_version(),
    pandoc = pandoc_version()
  )
}


# pandoc ------------------------------------------------------------------

pandoc_path <- function() {
  if (!is.na(path <- Sys.getenv("RSTUDIO_PANDOC", unset = NA))) {
    return(file.path(path, "pandoc"))
  }

  if (nzchar(path <- Sys.which("pandoc"))) {
    return(unname(path))
  }

  NULL
}


pandoc_version <- function() {
  path <- pandoc_path()
  if (is.null(path)) {
    return(NULL)
  }

  local_pandoc_safe_environment()
  out <- system2(path, "--version", stdout = TRUE)
  if (!is.null(attr(out, "status"))) {
    return(NULL)
  }

  # From pandoc::pandoc_version()
  re <- "^pandoc(?:\\.exe)? ([\\d.]+).*$"
  if (!grepl(re, out[[1]], perl = TRUE)) {
    return(NULL)
  }

  version <- gsub(re, "\\1", out[[1]], perl = TRUE)
  if (grepl("-nightly-", out[[1]]))
    version <- paste(version, "9999", sep = ".")

  version
}

# https://github.com/rstudio/rmarkdown/blob/10863429024e236/R/pandoc.R#L720-L758
local_pandoc_safe_environment <- function(code, envir = parent.frame()) {

  if (renv_envvar_exists("LC_ALL")) {
    renv_scope_envvars(LC_ALL = NULL, envir = envir)
  }

  if (renv_envvar_exists("LC_CTYPE")) {
    renv_scope_envvars(LC_CTYPE = NULL, envir = envir)
  }

  if (renv_platform_linux()) {
    if (!renv_envvar_exists("HOME"))
      stop("The 'HOME' environment variable must be set before running Pandoc.")

    if (!renv_envvar_exists("LANG") || identical(Sys.getenv("LANG"), "en_US"))
      renv_scope_envvars(LANG = "en_US.UTF-8", envir = envir)
  }

  invisible()
}


# quarto ------------------------------------------------------------------

quarto_path <- function() {
  if (!is.na(path <- Sys.getenv("QUARTO_PATH", unset = NA))) {
    return(path)
  }

  if (nzchar(path <- Sys.which("quarto"))) {
    return(unname(path))
  }

  NULL
}

quarto_version <- function() {
  path <- quarto_path()
  if (is.null(path)) {
    return(NULL)
  }

  # from quarto::quarto_version
  out <- system2(path, "--version", stdout = TRUE)
    if (!is.null(attr(out, "status"))) {
    return(NULL)
  }

  out
}
