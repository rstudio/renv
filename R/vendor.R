
#' Vendor renv in an R Package
#'
#' Vendor a copy of `renv` in an \R package.
#'
#' Calling `renv::vendor()` will:
#'
#' - Compile a vendored copy of `renv` to `inst/vendor/renv.R`,
#' - Generate an `renv` auto-loader at `R/renv.R`.
#'
#' Using this, projects can take a dependency on `renv`, and use `renv`
#' internals, in a CRAN-compliant way. After vendoring `renv`, you can
#' use `renv` APIs in your package via the embedded `renv` environment;
#' for example, you could call the [renv::dependencies()] function with:
#'
#' ```
#' renv$dependencies()
#' ```
#'
#' Be aware that `renv` internals might change in future releases, so if you
#' need to rely on `renv` internal functions, we strongly recommend testing
#' your usages of these functions to avoid potential breakage.
#'
#' @param version The version of `renv` to vendor. If `NULL` (the default),
#'   the current version of `renv` will be used. Ignored if `sources`
#'   is non-`NULL`.
#'
#' @param repository The Git repository from which `renv` should be retrieved.
#'   `renv` will use `git clone <repository> --branch <version>` to download
#'   the required `renv` sources. Ignored if `sources` is non-`NULL`.
#'
#' @param sources The path to local `renv` sources to be vendored.
#'
#' @param project The project in which `renv` should be vendored.
#'
#' @keywords internal
#'
vendor <- function(version    = NULL,
                   repository = "https://github.com/rstudio/renv",
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
  sources <- sources %||% renv_vendor_sources(version, repository)

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
    # A vendored copy of `renv` was created at: %s
    # The `renv` auto-loader was generated at:  %s
    #
    # Please add `renv$initialize()` to your package's `.onLoad()`
    # to ensure that `renv` is initialized on package load.
    #
  ")

  blurb <- sprintf(template, renv_path_pretty(embed), renv_path_pretty(loader))
  writeLines(blurb)

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

  # replace '..imports..' with the 'utils' imports we use
  imports <- renv_vendor_imports()
  replacements <- list(imports = imports, version = renv_metadata_version())
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

  paste(c("list(", entries, "  )"), collapse = "\n")

}

renv_vendor_sources <- function(version, repository) {

  # move to temporary directory
  owd <- setwd(tempdir())
  on.exit(setwd(owd), add = TRUE)

  # resolve version
  version <- version %||% renv_package_version("renv")

  printf("# Cloning renv %s from %s ... ", version, repository)
  args <- c("clone", "--branch", version, "--depth", "1", repository)
  renv_system_exec(git(), args, action = "cloning renv")
  writef("Done!")
  path <- normalizePath("renv", winslash = "/", mustWork = TRUE)

  # make sure we clean up when we're done
  defer(unlink(path, recursive = TRUE), envir = parent.frame())

  path

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
