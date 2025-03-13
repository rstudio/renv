
#' Find R package dependencies in a project
#'
#' @description
#' `dependencies()` will scan files within your project, looking for \R files
#' and the packages used within those \R files. This is done primarily by
#' parsing the code and looking for calls of the form `library(package)`,
#' `require(package)`, `requireNamespace("package")`, and `package::method()`.
#' renv also supports package loading with
#' [box](https://cran.r-project.org/package=box) (`box::use(...)`) and
#' [pacman](https://cran.r-project.org/package=pacman) (`pacman::p_load(...)`).
#'
#' For \R package projects, `renv` will also detect dependencies expressed
#' in the `DESCRIPTION` file. For projects using Python, \R dependencies within
#' the \R code chunks of your project's `.ipynb` files will also be used.
#'
#' Note that the \code{\link[rmarkdown:rmarkdown-package]{rmarkdown}} package is
#' required in order to scan dependencies in R Markdown files.
#'
#' # Missing dependencies
#'
#' `dependencies()` uses static analysis to determine which packages are used
#' by your project. This means that it inspects, but doesn't run, the \R code
#' in your project. Static analysis generally works well, but is not
#' 100% reliable in detecting the packages required by your project. For
#' example, `renv` is unable to detect this kind of usage:
#'
#' ```{r eval=FALSE}
#' for (package in c("dplyr", "ggplot2")) {
#'   library(package, character.only = TRUE)
#' }
#' ```
#'
#' It also can't generally tell if one of the packages you use, uses one of
#' its suggested packages. For example, the `tidyr::separate_wider_delim()`
#' function requires the `stringr` package, but `stringr` is only suggested,
#' not required, by `tidyr`.
#'
#' If you find that renv's dependency discovery misses one or more packages
#' that you actually use in your project, one escape hatch is to include a file
#' called `_dependencies.R` that includes straightforward library calls:
#'
#' ```
#' library(dplyr)
#' library(ggplot2)
#' library(stringr)
#' ```
#'
#' # Ignoring files
#'
#' By default, renv will read your project's `.gitignore`s (if present) to
#' determine whether certain files or folders should be included when traversing
#' directories. If preferred, you can also create a `.renvignore` file (with
#' entries of the same format as a standard `.gitignore` file) to tell renv
#' which files to ignore within a directory. If both `.renvignore` and
#' `.gitignore` exist within a folder, the `.renvignore` will be used in lieu of
#' the `.gitignore`.
#'
#' See <https://git-scm.com/docs/gitignore> for documentation on the
#' `.gitignore` format. Some simple examples here:
#'
#' ```
#' # ignore all R Markdown files
#' *.Rmd
#'
#' # ignore all data folders
#' data/
#'
#' # ignore only data folders from the root of the project
#' /data/
#' ```
#'
#' Using ignore files is important if your project contains a large number
#' of files; for example, if you have a `data/` directory containing many
#' text files.
#'
#'
#' ## Profile-specific Ignore Rules
#'
#' Profile-specific sections are also supported in `.renvignore` files.
#' These sections are marked with a comment header of the form `#| <code>`,
#' where `<code>` is \R code that indicates if this section of the `.renvignore`
#' should apply. The `profile` variable is set to the same value as the current
#' profile, or `"default"` if the default profile (no profile) is selected.
#' See `vignette("profiles", package = "renv")` for more information on profiles.
#'
#' ```
#' # ignore all directories by default
#' */
#'
#' #| profile == "default"
#' !default
#'
#' #| profile == "extra"
#' !extra
#' ```
#'
#' Note that the first section in a `.renvignore` file implicitly applies to
#' all profiles.
#'
#'
#' # Errors
#'
#' renv's attempts to enumerate package dependencies in your project can fail
#' -- most commonly, because of failures when attempting to parse your \R code.
#' You can use the `errors` argument to suppress these problems, but a
#' more robust solution is tell renv not to look at the problematic code.
#' As well as using `.renvignore`, as described above, you can also suppress errors
#' discovered within individual `.Rmd` chunks by including `renv.ignore=TRUE`
#' in the chunk header. For example:
#'
#'     ```{r chunk-label, renv.ignore=TRUE}
#'     # code in this chunk will be ignored by renv
#'     ```
#'
#' Similarly, if you'd like renv to parse a chunk that is otherwise ignored
#' (e.g. because it has `eval=FALSE` as a chunk header), you can set:
#'
#'     ```{r chunk-label, eval=FALSE, renv.ignore=FALSE}
#'     # code in this chunk will _not_ be ignored
#'     ```
#'
#' # Development dependencies
#'
#' renv has some support for distinguishing between development and run-time
#' dependencies. For example, your Shiny app might rely on
#' [ggplot2](https://ggplot2.tidyverse.org) (a run-time dependency) but while
#' you use [usethis](https://usethis.r-lib.org) during development, your app
#' doesn't need it to run (i.e. it's only a development dependency).
#'
#' You can record development dependencies by listing them in the `Suggests`
#' field of your project's `DESCRIPTION` file. Development dependencies will be installed by
#' [renv::install()] (when called without arguments) but will not be tracked in
#' the project snapshot. If you need greater control, you can also try project
#' profiles as discussed in `vignette("profiles")`.
#'
#' @inheritParams renv-params
#'
#' @param path The path to a `.R`, `.Rmd`, `.qmd`, `DESCRIPTION`, a directory
#'   containing such files, or an R function. The default uses all files
#'   found within the current working directory and its children.
#'
#' @param root The root directory to be used for dependency discovery.
#'   Defaults to the active project directory. You may need to set this
#'   explicitly to ensure that your project's `.renvignore`s (if any) are
#'   properly handled.
#'
#' @param quiet Boolean; be quiet while checking for dependencies?
#'   Setting `quiet = TRUE` is equivalent to setting `progress = FALSE`
#'   and `errors = "ignored"`, and overrides those options when not `NULL`.
#'
#' @param progress Boolean; report progress output while enumerating
#'   dependencies?
#'
#' @param errors How should errors that occur during dependency enumeration be
#'   handled?
#'
#'   * `"reported"` (the default): errors are reported to the user, but
#'      otherwise ignored.
#'   * `"fatal"`: errors are fatal and stop execution.
#'   *  `"ignored"`: errors are ignored and not reported to the user.
#'
#' @param dev Boolean; include development dependencies? These packages are
#'   typically required when developing the project, but not when running it
#'   (i.e. you want them installed when humans are working on the project but
#'   not when computers are deploying it).
#'
#'   Development dependencies include packages listed in the `Suggests` field
#'   of a `DESCRIPTION` found in the project root, and roxygen2 or devtools if
#'   their use is implied by other project metadata. They also include packages
#'   used in `~/.Rprofile` if `config$user.profile()` is `TRUE`.
#'
#' @return An \R `data.frame` of discovered dependencies, mapping inferred
#'   package names to the files in which they were discovered. Note that the
#'   `Package` field might name a package remote, rather than just a plain
#'   package name.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # find R package dependencies in the current directory
#' renv::dependencies()
#'
#' }
dependencies <- function(
  path = getwd(),
  root = NULL,
  ...,
  quiet = NULL,
  progress = TRUE,
  errors = c("reported", "fatal", "ignored"),
  dev = FALSE)
{
  renv_scope_error_handler()

  # special case: if 'path' is a function, parse its body for dependencies
  if (is.function(path))
    return(renv_dependencies_discover_r(expr = body(path)))

  renv_dependencies_impl(
    path     = path,
    root     = root,
    quiet    = quiet,
    progress = progress,
    errors   = errors,
    dev      = dev,
    ...
  )
}

renv_dependencies_impl <- function(
  path = getwd(),
  ...,
  root = NULL,
  field = NULL,
  quiet = NULL,
  progress = FALSE,
  errors = c("reported", "fatal", "ignored"),
  dev = FALSE)
{
  renv_dots_check(...)

  path <- renv_path_normalize(path, mustWork = TRUE)
  root <- root %||% renv_dependencies_root(path)

  # handle 'quiet' parameter
  if (quiet %||% FALSE) {
    progress <- FALSE
    errors <- "ignored"
  }

  # ignore errors when testing, unless explicitly asked for
  if (renv_tests_running() && missing(errors))
    errors <- "ignored"

  # resolve errors
  errors <- match.arg(errors)

  # the path to the user .Rprofile is used when discovering dependencies,
  # so resolve that eagerly now
  renv_scope_binding(
    envir       = the$paths,
    symbol      = "r_profile_user",
    replacement = Sys.getenv("R_PROFILE_USER", unset = path.expand("~/.Rprofile"))
  )

  before <- Sys.time()
  renv_dependencies_scope(root = root)
  files <- renv_dependencies_find(path, root)
  deps <- renv_dependencies_discover(files, progress, errors)
  after <- Sys.time()
  elapsed <- difftime(after, before, units = "secs")

  renv_condition_signal("renv.dependencies.elapsed_time", elapsed)
  renv_dependencies_report(errors)

  if (empty(deps) || nrow(deps) == 0L) {
    result <- renv_dependencies_list_empty()
    return(take(result, field))
  }

  # drop other NAs, just in case -- this really is an issue in the underlying
  # dependency computation code somewhere, but we still want to insulate users
  # from unexpected errors
  #
  # https://github.com/rstudio/renv/issues/2110
  keep <- !is.na(deps$Package) & deps$Dev %in% c(dev, FALSE)
  take(rows(deps, keep), field)
}

renv_dependencies_root <- function(path = getwd()) {

  path <- renv_path_normalize(path, mustWork = TRUE)

  project <- renv_project_get(default = NULL)
  if (!is.null(project) && all(renv_path_within(path, project)))
    return(project)

  roots <- uapply(path, renv_dependencies_root_impl)
  if (empty(roots))
    return(NULL)

  uniroot <- unique(roots)
  if (length(uniroot) > 1)
    return(NULL)

  uniroot

}

renv_dependencies_root_impl <- function(path) {

  renv_file_find(path, function(parent) {
    anchors <- c("DESCRIPTION", ".git", ".Rproj.user", "renv.lock", "renv")
    for (anchor in anchors)
      if (file.exists(file.path(parent, anchor)))
        return(parent)
  })

}

renv_dependencies_callback <- function(path) {

  cbname <- list(
    ".lintr"        = function(path) renv_dependencies_discover_lintr(path),
    ".Rprofile"     = function(path) renv_dependencies_discover_r(path),
    "DESCRIPTION"   = function(path) renv_dependencies_discover_description(path),
    "NAMESPACE"     = function(path) renv_dependencies_discover_namespace(path),
    "_bookdown.yml" = function(path) renv_dependencies_discover_bookdown(path),
    "_pkgdown.yml"  = function(path) renv_dependencies_discover_pkgdown(path),
    "_quarto.yml"   = function(path) renv_dependencies_discover_quarto(path),
    "renv.lock"     = function(path) renv_dependencies_discover_renv_lock(path),
    "rsconnect"     = function(path) renv_dependencies_discover_rsconnect(path)
  )

  cbext <- list(
    ".rproj"       = function(path) renv_dependencies_discover_rproj(path),
    ".r"           = function(path) renv_dependencies_discover_r(path),
    ".qmd"         = function(path) renv_dependencies_discover_multimode(path, "qmd"),
    ".rmd"         = function(path) renv_dependencies_discover_multimode(path, "rmd"),
    ".rmarkdown"   = function(path) renv_dependencies_discover_multimode(path, "rmd"),
    ".rnw"         = function(path) renv_dependencies_discover_multimode(path, "rnw"),
    ".ipynb"       = function(path) renv_dependencies_discover_ipynb(path)
  )

  name <- basename(path)
  ext  <- tolower(fileext(path))

  callback <- cbname[[name]] %||% cbext[[ext]]
  if (!is.null(callback))
    return(callback)

  # for files without an extension, check if those might be executable by R
  if (!nzchar(ext)) {
    shebang <- renv_file_shebang(path)
    if (grepl("\\b(?:R|r|Rscript)\\b", shebang))
      return(function(path) renv_dependencies_discover_r(path))
  }

}

renv_dependencies_find_extra <- function(root) {

  # if we don't have a root, we don't have a project
  if (is.null(root))
    return(NULL)

  # only run for root-level dependency checks
  project <- renv_project_resolve()
  if (!renv_path_same(root, project))
    return(NULL)

  # only run if we have a custom profile
  profile <- renv_profile_get()
  if (is.null(profile))
    return(NULL)

  # look for dependencies in the associated 'renv' folder
  path <- renv_paths_renv(project = project)
  renv_dependencies_find_impl(path, root, 0L)

}

renv_dependencies_find <- function(path = getwd(), root = getwd()) {
  files <- lapply(path, renv_dependencies_find_impl, root = root, depth = 0)
  extra <- renv_dependencies_find_extra(root)

  if (config$user.profile()) {
    profile <- the$paths$r_profile_user
    if (file.exists(profile)) {
      extra <- c(extra, profile)
    }
  }

  unlist(c(files, extra), recursive = TRUE, use.names = FALSE)
}

renv_dependencies_find_impl <- function(path, root, depth) {

  # check file type
  info <- renv_file_info(path)

  # the file might have been removed after listing -- if so, just ignore it
  if (is.na(info$isdir))
    return(NULL)

  # if this is a directory, recurse
  if (info$isdir)
    return(renv_dependencies_find_dir(path, root, depth))

  path
}

renv_dependencies_find_dir <- function(path, root, depth) {

  # check if this path should be ignored
  excluded <- renv_renvignore_exec(path, root, path)
  if (excluded)
    return(character())

  # check if we've already scanned this directory
  # (necessary to guard against recursive symlinks)
  if (!renv_platform_windows()) {
    norm <- renv_path_normalize(path)
    state <- renv_dependencies_state()
    if (visited(norm, state$scanned))
      return(character())
  }

  # list children
  children <- renv_dependencies_find_dir_children(path, root, depth)

  # notify about number of children
  renv_condition_signal("renv.dependencies.count", list(path = path, count = length(children)))

  # find recursive dependencies
  depth <- depth + 1
  paths <- map(children, renv_dependencies_find_impl, root = root, depth = depth)

  # explicitly include rsconnect folder
  # (so we can infer a dependency on rsconnect when appropriate)
  rsconnect <- file.path(path, "rsconnect")
  if (file.exists(rsconnect))
    paths <- c(rsconnect, paths)

  paths

}

# return the set of files / subdirectories within a directory that should be
# crawled for dependencies
renv_dependencies_find_dir_children <- function(path, root, depth) {

  # list files in the folder
  children <- renv_file_list(path, full.names = TRUE)

  # skip if this contains too many files
  # https://github.com/rstudio/renv/issues/1186
  count <- length(children)
  if (count >= config$dependencies.limit()) {
    relpath <- renv_path_relative(path, dirname(root))
    renv_dependencies_find_dir_children_overload(relpath, count)
  }

  # remove files which are broken symlinks
  children <- children[file.exists(children)]

  # remove hard-coded ignores
  # (only keep DESCRIPTION files at the top level)
  ignored <- c("packrat", "renv", "revdep", "vendor", if (depth) c("DESCRIPTION", "NAMESPACE"))
  children <- children[!basename(children) %in% ignored]

  # compute exclusions
  excluded <- renv_renvignore_exec(path, root, children)

  # keep only non-excluded children
  children[!excluded]

}

renv_dependencies_find_dir_children_overload <- function(path, count) {

  # check for missing state (e.g. if internal method called directly)
  state <- renv_dependencies_state()
  if (is.null(state))
    return()

  fmt <- "directory contains %s; consider ignoring this directory"
  msg <- sprintf(fmt, nplural("file", count))
  error <- simpleError(message = msg)

  path <- path %||% state$path
  problem <- list(file = path, error = error)
  state$problems$push(problem)

}

renv_dependencies_discover <- function(paths, progress, errors) {

  if (!renv_dependencies_discover_preflight(paths, errors))
    return(invisible(list()))

  # short path if we're not showing progress
  if (identical(progress, FALSE))
    return(bapply(paths, renv_dependencies_discover_impl))

  # otherwise, run with progress reporting

  # nocov start
  printf("Finding R package dependencies ... ")
  callback <- renv_progress_callback(renv_dependencies_discover_impl, length(paths))
  deps <- lapply(paths, callback)
  writef("Done!")

  bind(deps)
  # nocov end

}

renv_dependencies_discover_impl <- function(path) {

  callback <- renv_dependencies_callback(path)
  if (is.null(callback)) {
    return(NULL)
  }

  status <- catch(filebacked("dependencies", path, callback))
  if (inherits(status, "error")) {
    signalCondition(warnify(status))
    NULL
  }

  status

}

renv_dependencies_discover_preflight <- function(paths, errors) {

  if (identical(errors, "ignored"))
    return(TRUE)

  if (length(paths) < config$dependencies.limit())
    return(TRUE)

  lines <- c(
    "A large number of files (%i in total) have been discovered.",
    "It may take renv a long time to scan these files for dependencies.",
    "Consider using .renvignore to ignore irrelevant files.",
    "See `?renv::dependencies` for more information.",
    "Set `options(renv.config.dependencies.limit = Inf)` to disable this warning.",
    ""
  )
  writef(lines, length(paths))

  if (identical(errors, "reported"))
    return(TRUE)

  cancel_if(interactive() && !proceed())

  TRUE

}

renv_dependencies_discover_renv_lock <- function(path) {
  renv_dependencies_list(path, "renv")
}

renv_dependencies_discover_description_fields <- function(path, project = NULL) {

  # most callers don't pass in project so we need to get it from global state
  project <- project %||%
    renv_dependencies_state(key = "root") %||%
    renv_restore_state(key = "root") %||%
    renv_project_resolve()

  # by default, respect fields defined in settings
  fields <- settings$package.dependency.fields(project = project)

  # if this appears to be the DESCRIPTION associated with the active project,
  # and an explicit set of dependencies was provided in install, then use those
  if (renv_path_same(file.path(project, "DESCRIPTION"), path)) {
    default <- the$install_dependency_fields %||% c(fields, "Suggests")
    profile <- sprintf("Config/renv/profiles/%s/dependencies", renv_profile_get())
    fields <- c(default, profile)
  }

  fields

}

renv_dependencies_discover_description <- function(path,
                                                   fields = NULL,
                                                   subdir = NULL,
                                                   project = NULL)
{
  dcf <- catch(renv_description_read(path = path, subdir = subdir))
  if (inherits(dcf, "error"))
    return(renv_dependencies_error(path, error = dcf))

  # resolve the dependency fields to be used
  fields <- fields %||% renv_dependencies_discover_description_fields(path, project)

  # make sure dependency fields are expanded
  fields <- renv_description_dependency_fields_expand(fields)

  data <- map(
    fields,
    renv_dependencies_discover_description_impl,
    dcf  = dcf,
    path = path
  )

  names(data) <- fields

  # if this is a bioconductor package, add their implicit dependencies
  if ("biocViews" %in% names(dcf)) {
    data[[length(data) + 1L]] <- renv_dependencies_list(
      source = path,
      packages = c(renv_bioconductor_manager(), "BiocVersion")
    )
    names(data)[[length(data)]] <- "Bioconductor"
  }

  bind(data, index = "Type")
}

renv_dependencies_discover_namespace <- function(path) {

  tryCatch(
    renv_dependencies_discover_namespace_impl(path),
    error = warnify
  )

}

renv_dependencies_discover_namespace_impl <- function(path) {

  # parseNamespaceFile() expects to be called on an installed package,
  # so we have to pretend our best here
  library <- dirname(dirname(path))
  package <- basename(dirname(path))
  info <- parseNamespaceFile(
    package     = package,
    package.lib = library,
    mustExist   = TRUE
  )

  # read package names from imports
  packages <- map_chr(info$imports, `[[`, 1L)

  renv_dependencies_list(
    source   = path,
    packages = sort(unique(packages))
  )

}

renv_dependencies_discover_description_impl <- function(dcf, field, path) {

  # read field
  contents <- dcf[[field]]
  if (!is.character(contents))
    return(list())

  # split on commas
  parts <- strsplit(dcf[[field]], "\\s*,\\s*")[[1]]

  # drop any empty fields
  x <- parts[nzchar(parts)]

  # match to split on remote, version
  pattern <- paste0(
    "([^,\\([:space:]]+)",                    # remote name
    "(?:\\s*\\(([><=]+)\\s*([0-9.-]+)\\))?"   # optional version specification
  )

  m <- regexec(pattern, x)
  matches <- regmatches(x, m)
  if (empty(matches))
    return(list())

  # drop R (https://github.com/rstudio/renv/issues/1806)
  matches <- filter(matches, function(match) {
    !identical(match[[2L]], "R")
  })

  if (empty(matches))
    return(list())

  # create dependency list
  renv_dependencies_list(
    path,
    extract_chr(matches, 2L),
    extract_chr(matches, 3L),
    extract_chr(matches, 4L),
    dev = field == "Suggests"
  )

}

renv_dependencies_discover_bookdown <- function(path) {
  # TODO: other dependencies to parse from bookdown?
  renv_dependencies_list(path, "bookdown")
}

renv_dependencies_discover_pkgdown <- function(path) {
  # TODO: other dependencies to parse from pkgdown?
  renv_dependencies_list(path, "pkgdown")
}

renv_dependencies_discover_quarto <- function(path) {
  # TODO: other dependencies to parse from quarto?
  #
  # NOTE: we previously inferred a dependency on the R 'quarto' package here,
  # but quarto is normally invoked directly (rather than via the package) and
  # so such a dependency is not strictly necessary.
  #
  # https://github.com/rstudio/renv/issues/995
  renv_dependencies_list_empty()
}

renv_dependencies_discover_rsconnect <- function(path) {
  renv_dependencies_list(path, "rsconnect")
}

renv_dependencies_discover_multimode <- function(path, mode) {

  # TODO: find in-line R code?
  deps <- stack()

  if (mode %in% c("rmd", "qmd"))
    deps$push(renv_dependencies_discover_rmd_yaml_header(path, mode))

  deps$push(renv_dependencies_discover_chunks(path, mode))

  bind(Filter(NROW, deps$data()))

}

renv_dependencies_discover_rmd_yaml_header <- function(path, mode) {

  deps <- stack(mode = "character")

  # R Markdown documents always depend on rmarkdown
  if (identical(mode, "rmd"))
    deps$push("rmarkdown")

  # try and read the document's YAML header
  contents <- renv_file_read(path)
  pattern <- "(?:^|\n)\\s*---\\s*(?:$|\n)"
  matches <- gregexpr(pattern, contents, perl = TRUE)[[1L]]

  # check that we have something that looks like a YAML header
  ok <- length(matches) > 1L && matches[[1L]] == 1L
  if (!ok)
    return(renv_dependencies_list(path, packages = deps$data()))

  # require yaml package for parsing YAML header
  name <- case(
    mode == "rmd" ~ "R Markdown",
    mode == "qmd" ~ "Quarto Markdown"
  )

  # validate that we actually have the yaml package available
  if (!renv_dependencies_require("yaml", name)) {
    packages <- deps$data()
    return(renv_dependencies_list(path, packages))
  }

  # extract YAML text
  yamltext <- substring(contents, matches[[1L]] + 4L, matches[[2L]] - 1L)
  yaml <- catch(renv_yaml_load(yamltext))
  if (inherits(yaml, "error"))
    return(renv_dependencies_error(path, error = yaml, packages = "rmarkdown"))

  # check for Shiny runtime
  runtime <- yaml[["runtime"]] %||% ""
  if (pstring(runtime) && grepl("shiny", runtime, fixed = TRUE))
    deps$push("shiny")

  server <- yaml[["server"]] %||% ""
  if (identical(server, "shiny"))
    deps$push("shiny")

  if (is.list(server) && identical(server[["type"]], "shiny"))
    deps$push("shiny")

  pattern <- renv_regexps_package_name()

  # check recursively for package usages of the form 'package::method'
  recurse(yaml, function(node) {
    # look for keys of the form 'package::method'
    values <- c(names(node), if (pstring(node)) node)
    for (value in values) {
      call <- tryCatch(parse(text = value)[[1]], error = function(err) NULL)
      if (renv_call_matches(call, names = c("::", ":::"), nargs = 2L)) {
        deps$push(as.character(call[[2L]]))
      }
    }

  })

  # check for dependency on bslib
  theme <- catchall(yaml[[c("output", "html_document", "theme")]])
  if (!inherits(theme, "error") && is.list(theme))
    deps$push("bslib")

  # check for parameterized documents
  status <- catch(renv_dependencies_discover_rmd_yaml_header_params(yaml, deps))
  if (inherits(status, "error"))
    renv_dependencies_error_push(path, status)

  # get list of dependencies
  packages <- deps$data()
  renv_dependencies_list(path, packages)

}

renv_dependencies_discover_rmd_yaml_header_params <- function(yaml, deps) {

  # check for declared params
  params <- yaml[["params"]]
  if (!is.list(params))
    return()

  # infer dependency on shiny
  deps$push("shiny")

  # iterate through params, parsing dependencies from R code
  for (param in params) {

    # check for r types
    type <- attr(param, "type", exact = TRUE)
    if (!identical(type, "r"))
      next

    # attempt to parse dependencies
    rdeps <- catch(renv_dependencies_discover_r(text = param))
    if (inherits(rdeps, "error"))
      next

    # add each dependency
    for (package in sort(unique(rdeps$Package)))
      deps$push(package)

  }

}

renv_dependencies_discover_chunks_ignore <- function(chunk) {

  # if renv.ignore is set, respect it
  ignore <- chunk$params[["renv.ignore"]]
  if (!is.null(ignore))
    return(truthy(ignore))

  # skip non-R chunks
  engine <- chunk$params[["engine"]]
  ok <- is.character(engine) && tolower(engine) %in% c("r", "rscript")
  if (!ok)
    return(TRUE)

  # skip un-evaluated chunks
  if (!truthy(chunk$params[["eval"]], default = TRUE))
    return(TRUE)

  # skip learnr exercises
  if (truthy(chunk$params[["exercise"]], default = FALSE))
    return(TRUE)

  # skip chunks whose labels end in '-display'
  label <- chunk$params[["label"]] %||% ""
  if (grepl("-display$", label))
    return(TRUE)

  # ok, don't ignore this chunk
  FALSE

}

renv_dependencies_discover_chunks <- function(path, mode) {

  # figure out the appropriate begin, end patterns
  type <- tolower(file_ext(path))
  if (type %in% c("rmd", "qmd", "rmarkdown"))
    type <- "md"

  allpatterns <- renv_knitr_patterns()
  patterns <- allpatterns[[type]]
  if (is.null(patterns)) {
    condition <- simpleCondition("not a recognized multi-mode R document")
    return(renv_dependencies_error(path, error = condition))
  }

  # parse the chunks within
  # NOTE: we need to proceed line-by-line since the chunk end pattern might
  # end chunks not started by the chunk begin pattern (sad face)
  encoding <- if (type == "md") "UTF-8" else "unknown"
  contents <- readLines(path, warn = FALSE, encoding = encoding)
  ranges <- renv_dependencies_discover_chunks_ranges(path, contents, patterns)

  # extract chunk code from the used ranges
  chunks <- .mapply(function(lhs, rhs) {

    # parse params in header
    header <- contents[[lhs]]
    params <- renv_knitr_options_header(header, type)

    # extract chunk contents (preserve newlines for nicer error reporting)
    range <- seq.int(lhs + 1, length.out = rhs - lhs - 1)
    code <- rep.int("", length(contents))
    code[range] <- contents[range]

    # also parse chunk options
    params <- overlay(params, renv_knitr_options_chunk(code))

    # return list of outputs
    list(params = params, code = code)

  }, ranges, NULL)

  # iterate over chunks, and attempt to parse dependencies from each
  cdeps <- bapply(chunks, function(chunk) {

    # check whether this chunk should be ignored
    if (renv_dependencies_discover_chunks_ignore(chunk))
      return(character())

    # remove reused chunk placeholders
    pattern <- "<<[^>]+>>"
    code <- gsub(pattern, "", chunk$code)

    # okay, now we can discover deps
    deps <- catch(renv_dependencies_discover_r(path = path, text = code))
    if (inherits(deps, "error"))
      return(renv_dependencies_error(path, error = deps))

    deps

  })

  # check for dependencies in inline chunks as well
  ideps <- renv_dependencies_discover_chunks_inline(path, contents)

  # if this is a .qmd, infer a dependency on rmarkdown if we have any R chunks
  qdeps <- NULL
  if (mode %in% "qmd") {
    for (chunk in chunks) {
      engine <- chunk$params[["engine"]]
      if (is.character(engine) && tolower(engine) %in% c("r", "rscript")) {
        qdeps <- renv_dependencies_list(path, "rmarkdown")
        break
      }
    }
  }

  # paste them all together
  deps <- bind(list(cdeps, ideps, qdeps))
  if (is.null(deps))
    return(deps)

  deps$Source <- path
  deps

}

renv_dependencies_discover_chunks_inline <- function(path, contents) {

  pasted <- paste(contents, collapse = "\n")
  matches <- gregexpr("`r ([^`]+)`", pasted, perl = TRUE)
  if (identical(c(matches[[1L]]), -1L))
    return(list())

  text <- unlist(regmatches(pasted, matches), use.names = FALSE, recursive = FALSE)
  code <- substring(text, 4L, nchar(text) - 1L)
  deps <- renv_dependencies_discover_r(path = path, text = code)
  if (inherits(deps, "error"))
    return(renv_dependencies_error(path, error = deps))

  deps

}

renv_dependencies_discover_chunks_ranges <- function(path, contents, patterns) {

  output <- list()

  chunk <- FALSE
  start <- 1; end <- 1
  for (i in seq_along(contents)) {

    line <- contents[[i]]

    if (chunk == FALSE && grepl(patterns$chunk.begin, line)) {
      chunk <- TRUE
      start <- i
      next
    }

    if (chunk == TRUE && grepl(patterns$chunk.begin, line)) {
      end <- i
      output[[length(output) + 1]] <- list(lhs = start, rhs = end)
      start <- i
      next
    }

    if (chunk == TRUE && grepl(patterns$chunk.end, line)) {
      chunk <- FALSE
      end <- i
      output[[length(output) + 1]] <- list(lhs = start, rhs = end)
      next
    }

  }

  if (chunk) {
    message <- sprintf("chunk starting on line %i is not closed", start)
    error <- simpleError(message)
    renv_dependencies_error(path, error = error)
  }

  bind(output)

}

renv_dependencies_discover_ipynb <- function(path) {

  json <- catch(renv_json_read(path))
  if (inherits(json, "error")) {
    info <- renv_file_info(path)
    if (!is.na(info$size) && info$size > 1)
      renv_dependencies_error(path, error = json)
  }

  if (!identical(json$metadata$kernelspec$language, "R"))
    return()

  deps <- stack()
  if (identical(json$metadata$kernelspec$name, "ir"))
    deps$push(renv_dependencies_list(path, "IRkernel"))

  for (cell in json$cells) {
    if (cell$cell_type != "code")
      next

    code <- paste0(cell$source, collapse = "")
    deps$push(renv_dependencies_discover_r(path, text = code))
  }

  bind(deps$data())

}

renv_dependencies_discover_rproj <- function(path) {

  props <- renv_properties_read(path)

  deps <- stack()
  if (identical(props$PackageUseDevtools, "Yes")) {
    deps$push("devtools")
    deps$push("roxygen2")
  }

  renv_dependencies_list(path, deps$data(), dev = TRUE)

}

renv_dependencies_discover_lintr <- function(path) {
  renv_dependencies_list(path, "lintr", dev = TRUE)
}

renv_dependencies_discover_r <- function(path  = NULL,
                                         text  = NULL,
                                         expr  = NULL,
                                         envir = NULL,
                                         dev   = NULL)
{
  expr <- case(
    is.function(expr)  ~ body(expr),
    is.language(expr)  ~ expr,
    is.character(expr) ~ catch(renv_parse_text(expr)),
    is.character(text) ~ catch(renv_parse_text(text)),
    is.character(path) ~ catch(renv_parse_file(path)),
    ~ stop("internal error")
  )

  if (inherits(expr, "error"))
    return(renv_dependencies_error(path, error = expr))

  # resolve dev
  dev <- dev %||% path == the$paths$r_profile_user

  # update current path
  state <- renv_dependencies_state()
  if (!is.null(state))
    renv_scope_binding(state, "path", path)

  methods <- c(
    renv_dependencies_discover_r_methods,
    renv_dependencies_discover_r_xfun,
    renv_dependencies_discover_r_library_require,
    renv_dependencies_discover_r_require_namespace,
    renv_dependencies_discover_r_colon,
    renv_dependencies_discover_r_citation,
    renv_dependencies_discover_r_pacman,
    renv_dependencies_discover_r_modules,
    renv_dependencies_discover_r_import,
    renv_dependencies_discover_r_box,
    renv_dependencies_discover_r_targets,
    renv_dependencies_discover_r_glue,
    renv_dependencies_discover_r_ggplot2,
    renv_dependencies_discover_r_parsnip,
    renv_dependencies_discover_r_testthat,
    renv_dependencies_discover_r_knitr,
    renv_dependencies_discover_r_database
  )

  envir <- envir %||% new.env(parent = emptyenv())
  callback <- if (renv_ext_enabled()) {

    function(node) {
      node <- renv_call_normalize(node)
      for (method in methods)
        method(node, envir)
      invisible(node)
    }

  } else {

    function(node) {

      node <- renv_call_normalize(node)
      for (method in methods)
        method(node, envir)

      assign("object", node, envir = parent.frame())
      invisible(node)

    }

  }

  renv_dependencies_recurse(expr, callback)
  packages <- ls(envir = envir, all.names = TRUE)

  # also try to detect knitr::spin() dependencies -- this needs to
  # happen outside of the regular dependency discovery machinery
  # as it will rely on checking comments in the document
  #
  # https://github.com/rstudio/renv/issues/2023
  if (is.character(text) || is.character(path)) {
    text <- text %||% readLines(path, n = 1L, warn = FALSE)
    if (length(text) && grepl("^\\s*#'\\s*[-]{3}\\s*$", text[[1L]], perl = TRUE))
      packages <- union(c("knitr", "rmarkdown"), packages)
  }

  renv_dependencies_list(path, packages, dev = dev)
}

renv_dependencies_discover_r_methods <- function(node, envir) {

  node <- renv_call_expect(node, "methods", c("setClass", "setGeneric"))
  if (is.null(node))
    return(FALSE)

  envir[["methods"]] <- TRUE
  TRUE

}

renv_dependencies_discover_r_xfun <- function(node, envir) {

  node <- renv_call_expect(node, "xfun", c("pkg_attach", "pkg_attach2"))
  if (is.null(node))
    return(FALSE)

  # attempt to match the call
  prototype <- function(..., install = FALSE, message = TRUE) {}
  matched <- catch(match.call(prototype, node, expand.dots = FALSE))
  if (inherits(matched, "error"))
    return(FALSE)

  # extract character vectors from `...`
  strings <- stack()
  recurse(matched[["..."]], function(node) {
    if (is.character(node))
      strings$push(node)
  })

  # mark packages as known
  packages <- strings$data()
  if (empty(packages))
    return(FALSE)

  for (package in packages)
    envir[[package]] <- TRUE

  TRUE
}

renv_dependencies_discover_r_library_require <- function(node, envir) {

  node <- renv_call_expect(node, "base", c("library", "require"))
  if (is.null(node))
    return(FALSE)

  # attempt to match the call
  matched <- catch(match.call(base::library, node))
  if (inherits(matched, "error"))
    return(FALSE)

  # if the 'package' argument is a character vector of length one, we're done
  if (is.character(matched$package) &&
      length(matched$package) == 1)
  {
    envir[[matched$package]] <- TRUE
    return(TRUE)
  }

  # if it's a symbol, double check character.only argument
  if (is.symbol(matched$package) &&
      identical(matched$character.only %||% FALSE, FALSE))
  {
    envir[[as.character(matched$package)]] <- TRUE
    return(TRUE)
  }

  FALSE

}

renv_dependencies_discover_r_require_namespace <- function(node, envir) {

  node <- renv_call_expect(node, "base", c("requireNamespace", "loadNamespace"))
  if (is.null(node))
    return(FALSE)

  f <- get(as.character(node[[1]]), envir = .BaseNamespaceEnv, inherits = FALSE)
  matched <- catch(match.call(f, node))
  if (inherits(matched, "error"))
    return(FALSE)

  package <- matched$package
  if (is.character(package) && length(package == 1)) {
    envir[[package]] <- TRUE
    return(TRUE)
  }

  FALSE


}

renv_dependencies_discover_r_colon <- function(node, envir) {

  ok <- renv_call_matches(node, names = c("::", ":::"), nargs = 2L)
  if (!ok)
    return(FALSE)

  package <- node[[2L]]
  if (is.symbol(package))
    package <- as.character(package)

  if (!is.character(package) || length(package) != 1L)
    return(FALSE)

  envir[[package]] <- TRUE
  TRUE

}

renv_dependencies_discover_r_citation <- function(node, envir) {

  node <- renv_call_expect(node, "utils", "citation")
  if (is.null(node))
    return(FALSE)

  matched <- catch(match.call(utils::citation, node))
  if (inherits(matched, "error"))
    return(FALSE)

  package <- matched[["package"]]
  if (!is.character(package) || length(package) != 1L)
    return(FALSE)

  envir[[package]] <- TRUE
  TRUE

}

renv_dependencies_discover_r_pacman <- function(node, envir) {

  node <- renv_call_expect(node, "pacman", "p_load")
  if (is.null(node) || length(node) < 2)
    return(FALSE)

  # check for character.only
  chonly <- node[["character.only"]] %||% FALSE

  # consider all unnamed arguments
  parts <- as.list(node[-1L])

  # consider packages passed to 'char' parameter
  char <- node[["char"]]

  # detect vector of packages passed as vector
  if (renv_call_matches(char, "c"))
    parts <- c(parts, as.list(char[-1L]))

  # detect plain old package name
  if (is.character(char))
    parts <- c(parts, as.list(char))

  # ensure names
  names(parts) <- names(parts) %||% rep.int("", length(parts))
  unnamed <- parts[!nzchar(names(parts))]

  # extract symbols / characters
  for (arg in unnamed) {

    # skip symbols if necessary
    if (chonly && is.symbol(arg))
      next

    # check for character or symbol
    ok <-
      length(arg) == 1 &&
      is.character(arg) ||
      is.symbol(arg)

    if (!ok)
      next

    # add it
    envir[[as.character(arg)]] <- TRUE

  }

  TRUE

}

renv_dependencies_discover_r_modules <- function(node, envir) {

  # check for an explicit call to 'modules::import()'
  if (identical(node[[1L]], quote(modules::import))) {
    renv_dependencies_discover_r_modules_impl(node, envir)
  }

  # check for 'import' usages with a module block
  node <- renv_call_expect(node, "modules", "module")
  if (length(node) >= 2L &&
      identical(node[[1L]], quote(module)) &&
      is.call(node[[2L]]) &&
      identical(node[[2L]][[1L]], as.symbol("{")))
  {
    renv_dependencies_recurse(node[[2L]], function(node) {
      renv_dependencies_discover_r_modules_impl(node, envir)
    })
  }

}

renv_dependencies_discover_r_modules_impl <- function(node, envir) {

  node <- renv_call_expect(node, "modules", c("import"))
  if (is.null(node))
    return(FALSE)

  # attempt to match the call
  prototype <- function(from, ..., attach = TRUE, where = parent.frame()) {}
  matched <- catch(match.call(prototype, node, expand.dots = FALSE))
  if (inherits(matched, "error"))
    return(FALSE)

  # extract character vector or symbol from `from`
  package <- matched[["from"]]
  if (empty(package))
    return(FALSE)

  # package could be symbols or character so call as.character
  # to be safe then mark packages as known
  envir[[as.character(package)]] <- TRUE
  TRUE

}

renv_dependencies_discover_r_import <- function(node, envir) {

  # require that usages are colon-prefixed
  colon <- renv_call_matches(node[[1L]], names = c("::", ":::"), nargs = 2L)
  if (!colon)
    return(FALSE)

  node <- renv_call_expect(node, "import", c("from", "here", "into"))
  if (is.null(node))
    return(FALSE)

  # attempt to match the call
  name <- as.character(node[[1L]])
  matched <- if (name == "from") {
    catch(match.call(function(.from, ...) {}, node, expand.dots = FALSE))
  } else {
    catch(match.call(function(..., .from) {}, node, expand.dots = FALSE))
  }

  if (inherits(matched, "error"))
    return(FALSE)

  # the '.from' argument is the package name, either a character vector of length one or a symbol
  from <- matched$.from
  if (is.symbol(from)) {
    co <- node[[".character_only"]]
    if (!identical(co, TRUE))
      from <- as.character(from)
  }

  ok <- is.character(from) && length(from) == 1L
  if (!ok)
    return(FALSE)

  # '.from' can also be an R script; if it appears to be a path, then ignore it
  # https://github.com/rstudio/renv/issues/1743
  if (grepl("\\.[rR]$", from, perl = TRUE) &&
      grepl("[/\\]", from))
    return(FALSE)

  envir[[from]] <- TRUE
  TRUE

}

renv_dependencies_discover_r_box <- function(node, envir) {

  node <- renv_call_expect(node, "box", "use")
  if (is.null(node))
    return(FALSE)

  for (i in seq.int(2L, length.out = length(node) - 1L))
    renv_dependencies_discover_r_box_impl(node[[i]], envir)

  TRUE

}

renv_dependencies_discover_r_box_impl <- function(node, envir) {

  # if the call uses /, it's a path, not a package
  if (renv_call_matches(node, "/"))
    return(FALSE)

  # if the node is just a symbol, then it's the name of a package
  # otherwise, if it's a call to `[`, the first argument is the package name
  name <- if (is.symbol(node) && !identical(node, quote(expr = ))) {
    as.character(node)
  } else if (
    renv_call_matches(node, "[") &&
      length(node) > 1L &&
      is.symbol(node[[2L]])) {
    as.character(node[[2L]])
  }

  # the names `.` and `..` are special place holders and don't refer to packages
  if (is.null(name) || name == "." || name == "..")
    return(FALSE)

  envir[[name]] <- TRUE
  TRUE

}

renv_dependencies_discover_r_targets <- function(node, envir) {

  node <- renv_call_expect(node, "targets", "tar_option_set")
  if (is.null(node))
    return(FALSE)

  envir[["targets"]] <- TRUE

  packages <- tryCatch(
    renv_dependencies_eval(node$packages),
    error = identity
  )

  # TODO: evaluation can fail for a multitude of reasons;
  # are any of these worth signalling to the user?
  if (inherits(packages, "error"))
    return(TRUE)

  if (is.character(packages))
    for (package in packages)
      envir[[package]] <- TRUE

  TRUE

}

renv_dependencies_discover_r_glue <- function(node, envir) {

  node <- renv_call_expect(node, "glue", "glue")
  if (is.null(node))
    return(FALSE)

  # analyze all unnamed strings in the call
  args <- as.list(node)[-1L]
  nm <- names(args) %||% rep.int("", length(args))
  strings <- args[!nzchar(nm) & map_lgl(args, is.character)]

  # start iterating through the strings, looking for code chunks
  for (string in strings)
    renv_dependencies_discover_r_glue_impl(string, node, envir)

  TRUE

}

renv_dependencies_discover_r_ggplot2 <- function(node, envir) {

  node <- renv_call_expect(node, "ggplot2", "ggsave")
  if (is.null(node))
    return(FALSE)

  # check for attempts to save to '.svg', and assume svglite is
  # required in this scenario.
  matched <- catch(match.call(function(filename, ...) {}, node))
  if (inherits(matched, "error"))
    return(FALSE)

  filename <- matched$filename
  if (!is.character(filename))
    return(FALSE)

  if (!endswith(filename, ".svg"))
    return(FALSE)

  envir[["svglite"]] <- TRUE
  TRUE

}

renv_dependencies_discover_r_testthat <- function(node, envir) {

  # check for construction of JunitReporter
  if (identical(node, quote(JunitReporter$new))) {
    envir[["xml2"]] <- TRUE
    return(TRUE)
  }

  # check for an R6 class inheriting from a JunitReporter
  class <- renv_call_expect(node, "R6", "R6Class")
  if (!is.null(class) && identical(class$inherit, quote(JunitReporter))) {
    envir[["xml2"]] <- TRUE
    return(TRUE)
  }

  # check for calls to various test runners, which accept a reporter
  node <- renv_call_expect(node, "testthat", c("test_package", "test_dir", "test_file"))
  if (is.null(node))
    return(FALSE)

  candidates <- list(
    "Junit",
    "junit",
    quote(JunitReporter),
    quote(testthat::JunitReporter)
  )

  reporter <- node$reporter
  if (!is.null(reporter)) {
    for (candidate in candidates) {
      if (identical(candidate, reporter)) {
        envir[["xml2"]] <- TRUE
        return(TRUE)
      }
    }
  }

  FALSE

}

renv_dependencies_discover_r_knitr <- function(node, envir) {

  matched <- is.call(node) && (
    identical(node[[1L]], quote(knitr::opts_chunk$set)) ||
    identical(node[[1L]], quote(opts_chunk$set))
  )

  if (!matched)
    return(FALSE)

  args <- as.list(node)
  if (identical(args[["dev"]], "ragg_png")) {
    envir[["ragg"]] <- TRUE
    return(TRUE)
  }

  FALSE

}

renv_dependencies_discover_r_glue_impl <- function(string, node, envir) {

  # get open, close delimiters
  ropen    <- charToRaw(node$.open    %||% "{")
  rclose   <- charToRaw(node$.close   %||% "}")
  rcomment <- charToRaw(node$.comment %||% "#")

  # constants
  rcomment   <- charToRaw("#")
  rbackslash <- charToRaw("\\")
  rquotes <- c(
    charToRaw("'"),
    charToRaw("\""),
    charToRaw("`")
  )

  # iterate through characters in string
  raw <- c(charToRaw(string), as.raw(0L))
  i <- 0L
  n <- length(raw)
  quote <- raw()

  # index for open delimiter match
  index <- 0L
  count <- 0L

  while (i < n) {

    # ensure we always advance index
    i <- i + 1L

    # handle quoted states
    if (length(quote)) {

      # skip escaped characters
      if (raw[[i]] == rbackslash) {
        i <- i + 1L
        next
      }

      # check for escape from quote
      if (raw[[i]] == quote) {
        quote <- raw()
        next
      }

    }

    # skip comments
    if (raw[[i]] == rcomment) {
      i <- grepRaw("(?:$|\n)", raw, i)
      next
    }

    # skip escaped characters
    if (raw[[i]] == rbackslash) {
      i <- i + 1L
      next
    }

    # check for quotes
    idx <- match(raw[[i]], rquotes, nomatch = 0L)
    if (idx > 0) {
      quote <- rquotes[[idx]]
      next
    }

    # check for open delimiter
    if (i %in% grepRaw(ropen, raw, i, fixed = TRUE)) {

      # check for duplicate (escape)
      j <- i + length(ropen)
      if (j %in% grepRaw(ropen, raw, j, fixed = TRUE)) {
        i <- j + length(ropen) - 1L
        next
      }

      # save index if we're starting a match
      if (count == 0L) {
        index <- i
      }

      # increment match count
      count <- count + 1L
      next

    }

    # check for close delimiter
    if (i %in% grepRaw(rclose, raw, i, fixed = TRUE)) {

      # check for duplicate (escape)
      j <- i + length(rclose)
      if (j %in% grepRaw(rclose, raw, j, fixed = TRUE)) {
        i <- j + length(rclose) - 1L
        next
      }

      if (count > 0L) {

        # decrement count if we have a match
        count <- count - 1L

        # check for match and parse dependencies within
        if (count == 0L) {

          # extract inner code
          lhs <- index + length(ropen)
          rhs <- i - 1L
          code <- rawToChar(raw[lhs:rhs])

          # parse dependencies
          renv_dependencies_discover_r(text = code, envir = envir)

        }

      }

    }

  }

}

renv_dependencies_discover_r_parsnip <- function(node, envir) {

  node <- renv_call_expect(node, "parsnip", "set_engine")
  if (is.null(node))
    return(FALSE)

  matched <- catch(match.call(function(object, engine, ...) {}, node))
  if (inherits(matched, "error"))
    return(FALSE)

  engine <- matched$engine
  if (!is.character(engine) || length(engine) != 1L)
    return(FALSE)

  map <- getOption("renv.parsnip.engines", default = list(
    glm    = "stats",
    glmnet = "glmnet",
    keras  = "keras",
    kknn   = "kknn",
    nnet   = "nnet",
    rpart  = "rpart",
    spark  = "sparklyr",
    stan   = "rstanarm"
  ))

  packages <- if (is.function(map))
    tryCatch(map(engine), error = function(e) NULL)
  else
    map[[engine]]

  if (is.null(packages))
    return(FALSE)

  for (package in packages)
    envir[[package]] <- TRUE

  # TODO: a number of model routines appear to depend on dials;
  # should we just assume it's required by default? or should
  # users normally be using tidymodels instead of parsnip directly?
  TRUE

}

renv_dependencies_discover_r_database <- function(node, envir) {

  found <- FALSE

  matched <- function(requirements) {
    for (requirement in requirements)
      envir[[requirement]] <<- TRUE
    found <<- TRUE
  }

  db <- renv_dependencies_database()
  enumerate(db, function(package, dependencies) {
    enumerate(dependencies, function(method, requirements) {

      if (is.call(node)) {
        expect <- renv_call_expect(node, package, method)
        if (!is.null(expect))
          return(matched(requirements))
      }

      if (is.symbol(node)) {
        value <- as.character(node)
        if (identical(value, method))
          return(matched(requirements))
      }

    })
  })

  found

}

renv_dependencies_database <- function() {
  the$dependencies_database <- the$dependencies_database %||% {
    db <- getOption("renv.dependencies.database", default = list())
    db$ggplot2$geom_hex <- "hexbin"
    db$testthat$JunitReporter <- "xml2"
    db
  }
}

renv_dependencies_list <- function(source,
                                   packages,
                                   require = "",
                                   version = "",
                                   dev = FALSE)
{
  if (empty(packages))
    return(renv_dependencies_list_empty())

  source <- source %||% rep.int(NA_character_, length(packages))

  data_frame(
    Source  = as.character(source),
    Package = as.character(packages),
    Require = require,
    Version = version,
    Dev     = dev
  )

}

renv_dependencies_list_empty <- function() {

  data_frame(
    Source  = character(),
    Package = character(),
    Require = character(),
    Version = character(),
    Dev     = logical()
  )

}

renv_dependencies_require <- function(package, type = NULL) {

  if (requireNamespace(package, quietly = TRUE))
    return(TRUE)

  if (once()) {

    fmt <- lines(
      "The '%1$s' package is required to parse dependencies within %2$s",
      "Consider installing it with `install.packages(\"%1$s\")`."
    )

    within <- if (is.null(type)) "this project" else paste(type, "files")
    warningf(fmt, package, within)

  }

  return(FALSE)

}

the$dependencies_state <- NULL

renv_dependencies_state <- function(key = NULL) {
  state <- the$dependencies_state
  if (is.null(key)) state else state[[key]]
}

renv_dependencies_scope <- function(root = NULL, scope = parent.frame()) {
  state <- env(root = root, scanned = env(), problems = stack())
  the$dependencies_state <- state
  defer(the$dependencies_state <- NULL, scope = scope)
}

renv_dependencies_error_push <- function(path = NULL, error = NULL) {

  state <- renv_dependencies_state()
  if (is.null(state))
    return()

  path <- path %||% state$path
  problem <- list(file = path, error = error)
  state$problems$push(problem)

}

renv_dependencies_error <- function(path, error = NULL, packages = NULL) {

  # if no error, return early
  if (is.null(error))
    return(renv_dependencies_list(path, packages))

  # push the error report
  renv_dependencies_error_push(path, error)

  # return dependency list
  renv_dependencies_list(path, packages)

}

renv_dependencies_report <- function(errors) {

  if (identical(errors, "ignored"))
    return(FALSE)

  state <- renv_dependencies_state()
  if (is.null(state))
    return(FALSE)

  problems <- state$problems$data()
  if (empty(problems))
    return(TRUE)

  # bind into list
  bound <- bapply(problems, function(problem) {
    fields <- c(renv_path_aliased(problem$file), problem$line, problem$column)
    header <- paste(fields, collapse = ":")
    message <- conditionMessage(problem$error)
    c(file = problem$file, header = header, message = message)
  })

  # split based on header (group errors from same file)
  splat <- split(bound, bound$file)

  # emit messages
  lines <- enumerate(splat, function(file, problem) {
    messages <- paste("Error", problem$message, sep = ": ", collapse = "\n\n")
    paste(c(header(file), messages, ""), collapse = "\n")
  })

  bulletin(
    "WARNING: One or more problems were discovered while enumerating dependencies.",
    c("", lines),
    "Please see `?renv::dependencies` for more information.",
    bullets = FALSE
  )

  if (identical(errors, "fatal")) {
    fmt <- "one or more problems were encountered while enumerating dependencies"
    stopf(fmt)
  }

  renv_condition_signal("renv.dependencies.problems", problems)
  TRUE

}

renv_dependencies_eval <- function(expr) {

  # create environment with small subset of "safe" symbols, that
  # are commonly used for chunk expressions
  syms <- c(
    "list", "c", "T", "F",
    "{", "(", "[", "[[",
    "::", ":::", "$", "@",
    ":",
    "+", "-", "*", "/",
    "<", ">", "<=", ">=", "==", "!=",
    "!",
    "&", "&&", "|", "||"
  )

  vals <- mget(syms, envir = baseenv())
  envir <- list2env(vals, parent = emptyenv())

  # evaluate in that environment
  eval(expr, envir = envir)

}

renv_dependencies_recurse <- function(object, callback) {

  if (is.call(object))
    callback(object)

  if (is.recursive(object))
    for (i in seq_along(object))
      if (is.call(object[[i]]))
        renv_dependencies_recurse_impl(object[[i]], callback)

}

renv_dependencies_recurse_impl <- function(object, callback) {
  callback(object)
  for (i in seq_along(object))
    if (is.call(object[[i]]))
      renv_dependencies_recurse_impl(object[[i]], callback)
}
