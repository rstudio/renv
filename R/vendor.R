
#' Vendor renv in an R package
#'
#' @description
#' Calling `renv:::vendor()` will:
#'
#' - Compile a vendored copy of renv to `inst/vendor/renv.R`,
#' - Generate an renv auto-loader at `R/renv.R`.
#'
#' Using this, projects can take a dependency on renv, and use renv
#' internals, in a CRAN-compliant way. After vendoring renv, you can
#' use renv APIs in your package via the embedded renv environment;
#' for example, you could call the [renv::dependencies()] function with:
#'
#' ```
#' renv$dependencies()
#' ```
#'
#' Be aware that renv internals might change in future releases, so if you
#' need to rely on renv internal functions, we strongly recommend testing
#' your usages of these functions to avoid potential breakage.
#'
#' @param version The version of renv to vendor. `renv` sources will be pulled
#'   from GitHub, and so `version` should refer to either a commit hash or a
#'   branch name.
#'
#' @param project The project in which renv should be vendored.
#'
#' @keywords internal
#'
vendor <- function(version = "main", project = getwd()) {
  renv_scope_error_handler()

  # validate project is a package
  descpath <- file.path(project, "DESCRIPTION")
  if (!file.exists(descpath)) {
    fmt <- "%s does not contain a DESCRIPTION file; cannot proceed"
    stopf(fmt, renv_path_pretty(project))
  }

  # retrieve package sources
  sources <- renv_vendor_sources(version)

  # compute package remote
  spec <- sprintf("rstudio/renv@%s", version)
  remote <- renv_remotes_resolve(spec)

  # build script header
  header <- renv_vendor_header(remote)

  # create the renv script itself
  embed <- renv_vendor_create(
    project    = project,
    sources    = sources,
    header     = header
  )

  # create the loader
  loader <- renv_vendor_loader(project, remote, header)

  # let the user know what just happened
  template <- heredoc("
    #
    # A vendored copy of renv was created at: %s
    # The renv auto-loader was generated at:  %s
    #
    # Please add `renv$initialize(libname, pkgname)` to your package's
    # `.onLoad()` to ensure that renv is initialized on package load.
    #
  ")

  writef(template, renv_path_pretty(embed), renv_path_pretty(loader))

  invisible(TRUE)
}

renv_vendor_create <- function(project, sources, header) {

  # find all the renv R source scripts
  scripts <- list.files(file.path(sources, "R"), full.names = TRUE)

  # read into a single file
  contents <- map_chr(scripts, function(script) {
    header <- header(basename(script), n = 78L)
    contents <- readLines(script)
    parts <- c(header, "", contents, "", "")
    paste(parts, collapse = "\n")
  })

  # paste into single script
  bundle <- paste(contents, collapse = "\n")
  all <- c(header, "", bundle)

  # write to file
  target <- file.path(project, "inst/vendor/renv.R")
  ensure_parent_directory(target)
  writeLines(all, con = target)

  # return generated bundle
  invisible(target)

}

renv_vendor_loader <- function(project, remote, header) {

  source <- system.file("resources/vendor/renv.R", package = "renv")
  template <- readLines(source, warn = FALSE)

  # replace '..imports..' with the imports we use
  imports <- renv_vendor_imports()

  # create metadata for the embedded version
  version <- renv_metadata_version_create(remote)
  metadata <- renv_metadata_create(embedded = TRUE, version = version)

  # format metadata for template insertion
  lines <- enum_chr(metadata, function(key, value) {
    sprintf("    %s = %s", key, deparse(value))
  })

  inner <- paste(lines, collapse = ",\n")

  replacements <- list(
    imports  = imports,
    metadata = paste(c("list(", inner, "  )"), collapse = "\n")
  )
  contents <- renv_template_replace(template, replacements, format = "..%s..")

  all <- c("", header, "", contents)
  target <- file.path(project, "R/renv.R")
  ensure_parent_directory(target)
  writeLines(all, con = target)

  invisible(target)

}

renv_vendor_imports <- function() {

  imports <- getNamespaceImports("renv")

  # collect into sane format
  packages <- setdiff(unique(names(imports)), c("base", ""))
  names(packages) <- packages
  table <- map(packages, function(package) {
    unlist(imports[names(imports) == package], use.names = FALSE)
  })

  # format nicely
  entries <- enum_chr(table, function(package, functions) {
    lines <- sprintf("      \"%s\"", functions)
    body <- paste(lines, collapse = ",\n")
    parts <- c(sprintf("    %s = c(", package), body, "    )")
    paste(parts, collapse = "\n")
  })

  paste(c("list(", paste(entries, collapse = ",\n"), "  )"), collapse = "\n")

}

renv_vendor_sources <- function(version) {

  # retrieve renv
  tarball <- renv_bootstrap_download_github(version = version)

  # extract downloaded sources
  untarred <- tempfile("renv-vendor-")
  untar(tarball, exdir = untarred)

  # the package itself will exist as a folder within 'exdir'
  list.files(untarred, full.names = TRUE)[[1L]]

}

renv_vendor_header <- function(remote) {

  template <- heredoc("
    #
    # renv %s [rstudio/renv#%s]: A dependency management toolkit for R.
    # Generated using `renv:::vendor()` at %s.
    #
  ")

  version <- remote$Version
  hash <- substring(remote$RemoteSha, 1L, 7L)
  sprintf(template, version, hash, Sys.time())

}
