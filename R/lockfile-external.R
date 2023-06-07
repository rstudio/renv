renv_lockfile_external <- function() {
  list(
    quarto = quarto_version(),
    pandoc = pandoc_version()
  )
}

pandoc_path <- function() {
  if (!is.na(path <- Sys.getenv("RSTUDIO_PANDOC", unset = NA))) {
    return(file.path(path, "pandoc"))
  }

  if (nzchar(path <- Sys.which("pandoc"))) {
    return(unname(path))
  }

  NULL
}

quarto_path <- function() {
  if (!is.na(path <- Sys.getenv("QUARTO_PATH", unset = NA))) {
    return(path)
  }

  if (nzchar(path <- Sys.which("quarto"))) {
    return(unname(path))
  }

  NULL
}

pandoc_version <- function() {
  path <- pandoc_path()
  if (is.null(path)) {
    return(NULL)
  }

  out <- system2(path, "--version", stdout = TRUE)

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

quarto_version <- function() {
  path <- quarto_path()
  if (is.null(path)) {
    return(NULL)
  }

  # from quarto::quarto_version
  system2(path, "--version", stdout = TRUE)
}
