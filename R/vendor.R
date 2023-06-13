
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
#' @param version The version of renv to vendor. A Git tag targets a CRAN
#'   version and "main" references the latest development version of renv. Use
#'   a Git branch name or commit SHA to target other versions. Ignored when
#'   `sources` is non-`NULL`.
#'
#' @param sources The path to local renv sources to be vendored.
#'
#' @param project The project in which renv should be vendored.
#'
#' @keywords internal
#'
vendor <- function(version    = NULL,
                   sources    = NULL,
                   project    = getwd())
{
  renv_scope_error_handler()

  # validate project is a package
  descpath <- file.path(project, "DESCRIPTION")
  if (!file.exists(descpath)) {
    fmt <- "%s does not contain a DESCRIPTION file; cannot proceed"
    stopf(fmt, renv_path_pretty(project))
  }

  # get renv sources
  if (is.null(sources)) {
    if (is.null(version)) {
      stop("Specify an renv version or a path to renv sources")
    }
    sources <- renv_vendor_sources(version)
  }

  # re-compute renv version from sources
  version <- renv_description_read(path = sources, field = "Version")
  header <- renv_vendor_header(version)

  # create the renv script itself
  embed <- renv_vendor_create(
    project    = project,
    sources    = sources,
    header     = header
  )

  # create the loader
  loader <- renv_vendor_loader(project, header)

  # let the user know what just happened
  template <- heredoc("
    #
    # A vendored copy of renv was created at: %s
    # The renv auto-loader was generated at:  %s
    #
    # Please add `renv$initialize()` to your package's `.onLoad()`
    # to ensure that renv is initialized on package load.
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

renv_vendor_loader <- function(project, header) {

  source <- system.file("resources/vendor/renv.R", package = "renv")
  template <- readLines(source, warn = FALSE)

  # replace '..imports..' with the imports we use
  imports <- renv_vendor_imports()

  metadata <- as.list(`_renv_metadata`)
  metadata$embedded <- TRUE

  replacements <- list(
    imports = imports,
    metadata = deparse(metadata)
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

  tarball <- renv_bootstrap_download_github(version)
  if (!is.character(tarball) || !file.exists(tarball)) {
    stop("Download failed")
  }
  defer(unlink(tarball))

  untarred <- tempfile("renv-vendor-")
  untar(tarball, exdir = untarred)

  dir(untarred, full.names = TRUE)[[1]]
}

renv_vendor_header <- function(version) {

  template <- heredoc("
    #
    # renv %s: A dependency management toolkit for R.
    # Generated using `renv:::vendor()` at %s.
    #
  ")

  sprintf(template, version, Sys.time())

}
