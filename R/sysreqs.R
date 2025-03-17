
the$sysreqs <- NULL

#' R System Requirements
#'
#' Compute the system requirements (system libraries; operating system packages)
#' required by a set of \R packages.
#'
#' This function relies on the database of package system requirements
#' maintained by Posit at <https://github.com/rstudio/r-system-requirements>,
#' as well as the "meta-CRAN" service at <https://crandb.r-pkg.org>. This
#' service primarily exists to map the (free-form) `SystemRequirements` field
#' used by \R packages to the system packages made available by a particular
#' operating system.
#'
#' As an example, the `curl` R package depends on the `libcurl` system library,
#' and declares this with a `SystemRequirements` field of the form:
#'
#' - libcurl (>= 7.62): libcurl-devel (rpm) or libcurl4-openssl-dev (deb)
#'
#' This dependency can be satisfied with the following command line invocations
#' on different systems:
#'
#' - Debian: `sudo apt install libcurl4-openssl-dev`
#' - Redhat: `sudo dnf install libcurl-devel`
#'
#' and so `sysreqs("curl")` would help provide the name of the package
#' whose installation would satisfy the `libcurl` dependency.
#'
#'
#' @inheritParams renv-params
#'
#' @param packages A vector of \R package names. When `NULL`
#'   (the default), the project's package dependencies as reported via
#'   [renv::dependencies()] are used.
#'
#' @param local Boolean; should `renv` rely on locally-installed copies of
#'   packages when resolving system requirements? When `FALSE`, `renv` will
#'   use <https://crandb.r-pkg.org> to resolve the system requirements
#'   for these packages.
#'
#' @param check Boolean; should `renv` also check whether the requires system
#'   packages appear to be installed on the current system? Ignored when
#'   `distro` is supplied.
#'
#' @param report Boolean; should `renv` also report the commands which could be
#'   used to install all of the requisite package dependencies?
#'
#' @param collapse Boolean; when reporting which packages need to be installed,
#'   should the report be collapsed into a single installation command? When
#'   `FALSE` (the default), a separate installation line is printed for each
#'   required system package.
#'
#' @param distro The name of the Linux distribution for which system requirements
#'   should be checked -- typical values are "ubuntu", "debian", and "redhat".
#'   These should match the distribution names used by the R system requirements
#'   database. A version suffix can be included; for example, "ubuntu:24.04".
#'
#' @examples
#'
#' \dontrun{
#'
#' # report the required system packages for this system
#' sysreqs()
#'
#' # report the required system packages for a specific OS
#' sysreqs(platform = "ubuntu")
#'
#' }
#'
#' @export
sysreqs <- function(packages = NULL,
                    ...,
                    local    = FALSE,
                    check    = NULL,
                    report   = TRUE,
                    distro   = NULL,
                    collapse = FALSE,
                    project  = NULL)
{
  # allow user to provide additional package names as part of '...'
  if (!missing(...)) {
    dots <- list(...)
    names(dots) <- names(dots) %||% rep.int("", length(dots))
    packages <- c(packages, dots[!nzchar(names(dots))])
  }

  # resolve packages
  packages <- packages %||% {
    project <- renv_project_resolve(project)
    deps <- dependencies(project, dev = TRUE)
    sort(unique(deps$Package))
  }

  # remove 'base' packages
  base <- installed_packages(priority = "base")
  packages <- setdiff(packages, base$Package)
  names(packages) <- packages

  # resolve check
  check <- check %||% is.null(distro)

  # resolve distro
  distro <- distro %||% the$distro
  if (!identical(distro, the$distro)) {
    parts <- strsplit(distro, ":", fixed = TRUE)[[1L]]
    distro <- parts[[1L]]
    renv_scope_binding(the, "os", "linux")
    renv_scope_binding(the, "distro", parts[[1L]])
    renv_scope_binding(the, "platform", list(VERSION_ID = parts[[2L]]))
  }

  # compute package records
  if (local) {
    lockfile <- renv_lockfile_create(project, dev = TRUE)
    records <- renv_lockfile_records(lockfile)
  } else {
    callback <- renv_progress_callback(renv_sysreqs_crandb, length(packages))
    records <- map(packages, callback)
  }

  # extract and resolve the system requirements
  sysreqs <- map(records, `[[`, "SystemRequirements")
  sysdeps <- map(sysreqs, renv_sysreqs_resolve)

  # check the package status if possible
  if (check)
    renv_sysreqs_check(sysreqs, prompt = FALSE)

  # report installation commands if requested
  if (report)
    renv_sysreqs_report(sysdeps, distro, collapse)

  # return result
  invisible(sysdeps)

}

renv_sysreqs_report <- function(sysdeps, distro, collapse) {

  # collect all system packages
  syspkgs <- map(sysdeps, `[[`, "packages")
  allpkgs <- sort(unique(unlist(syspkgs)))
  if (empty(allpkgs))
    return()

  # include pre-install commands as well, if any
  preinstall <- unlist(map(sysdeps, `[[`, "pre_install"))
  if (length(preinstall)) {
    if (interactive()) {
      preamble <- "System pre-requisites can be installed with:"
      bulletin(preamble, preinstall)
    } else {
      writeLines(preinstall)
    }
  }

  # generate installation commands
  installer <- renv_sysreqs_installer(distro)
  body <- if (collapse) paste(allpkgs, collapse = " ") else allpkgs
  message <- paste("sudo", installer, "-y", body)

  if (interactive()) {
    preamble <- "The requisite system packages can be installed with:"
    bulletin(preamble, message)
  } else {
    writeLines(message)
  }

}

renv_sysreqs_crandb <- function(package) {
  tryCatch(
    renv_sysreqs_crandb_impl(package),
    error = warnify
  )
}

renv_sysreqs_crandb_impl <- function(package) {
  memoize(
    key   = package,
    value = renv_sysreqs_crandb_impl_one(package),
    scope = "sysreqs"
  )
}

renv_sysreqs_crandb_impl_one <- function(package) {
  url <- paste("https://crandb.r-pkg.org", package, sep = "/")
  destfile <- tempfile("renv-crandb-", fileext = ".json")
  download(url, destfile = destfile, quiet = TRUE)
  renv_json_read(destfile)
}

renv_sysreqs_resolve <- function(sysreqs, rules = renv_sysreqs_rules()) {
  matches <- map(sysreqs, renv_sysreqs_match, rules)
  unlist(matches, recursive = FALSE)
}

renv_sysreqs_read <- function(package) {
  desc <- renv_description_read(package)
  desc[["SystemRequirements"]] %||% ""
}

renv_sysreqs_rules <- function() {
  the$sysreqs <- the$sysreqs %||% renv_sysreqs_rules_impl()
}

renv_sysreqs_rules_impl <- function() {
  rules <- system.file("sysreqs/sysreqs.json", package = "renv")
  renv_json_read(rules)
}

renv_sysreqs_match <- function(sysreq, rules = renv_sysreqs_rules()) {

  for (rule in rules) {
    match <- renv_sysreqs_match_impl(sysreq, rule)
    if (!is.null(match)) {
      return(match)
    }
  }

}

renv_sysreqs_match_impl <- function(sysreq, rule) {

  # check for a match in the declared system requirements
  pattern <- paste(rule$patterns, collapse = "|")
  matches <- grepl(pattern, sysreq, ignore.case = TRUE, perl = TRUE)

  # if we got a match, pull out the dependent packages
  if (matches) {
    for (dependency in rule$dependencies) {
      for (constraint in dependency$constraints) {
        if (renv_sysreqs_satisfies(constraint)) {
          return(dependency)
        }
      }
    }
  }

}

renv_sysreqs_satisfies <- function(constraint) {

  if (constraint$os == the$os) {
    if (constraint$distribution == the$distro) {
      versions <- constraint$versions %||% the$platform$VERSION_ID
      for (version in versions) {
        if (startsWith(the$platform$VERSION_ID, version)) {
          return(TRUE)
        }
      }
    }
  }

  FALSE

}

renv_sysreqs_aliases <- function(type, syspkgs) {
  case(
    type == "deb" ~ renv_sysreqs_aliases_deb(syspkgs),
    type == "rpm" ~ renv_sysreqs_aliases_rpm(syspkgs)
  )
}

renv_sysreqs_aliases_deb <- function(pkgs) {

  # https://www.debian.org/doc/debian-policy/ch-relationships.html#s-virtual
  #
  # > A virtual package is one which appears in the Provides control field of
  # > another package. The effect is as if the package(s) which provide a
  # > particular virtual package name had been listed by name everywhere the
  # > virtual package name appears. (See also Virtual packages)
  #
  # read the package database, look which packages 'provide' others,
  # and then reverse that map to map virtual packages to the concrete
  # package which provides them
  #
  command <- "dpkg-query -W -f '${Package}=${Provides}\n'"
  output <- system(command, intern = TRUE)
  result <- renv_properties_read(text = output, delimiter = "=")

  # keep only packages which provide other packages
  aliases <- result[nzchar(result)]

  # a package might provide multiple other packages, so split those
  splat <- lapply(aliases, function(alias) {
    parts <- strsplit(alias, ",\\s*", perl = TRUE)[[1L]]
    names(renv_properties_read(text = parts, delimiter = " "))
  })

  # reverse the map, so that we can map virtual packages to the
  # concrete packages which they refer to
  envir <- new.env(parent = emptyenv())
  enumerate(splat, function(package, virtuals) {
    for (virtual in virtuals) {
      envir[[virtual]] <<- c(envir[[virtual]], package)
    }
  })

  # convert to intermediate list
  result <- as.list(envir, all.names = TRUE)

  # return as named character vector
  convert(result, type = "character")

}

renv_sysreqs_aliases_rpm <- function(pkgs) {

  # for each package, check if there's another package that 'provides' it
  fmt <- "rpm --query --whatprovides %s --queryformat '%%{Name}\n'"
  args <- paste(renv_shell_quote(pkgs), collapse = " ")
  command <- sprintf(fmt, args)
  result <- suppressWarnings(system(command, intern = TRUE))

  # return as named vector, mapping virtual packages to 'real' packages
  matches <- grep("no package provides", result, fixed = TRUE, invert = TRUE)
  aliases <- result[matches]
  names(aliases) <- pkgs[matches]

  convert(aliases, type = "character")

}

renv_sysreqs_check <- function(sysreqs, prompt) {

  # check for a supported package installer
  type <- case(
    nzchar(Sys.which("dpkg")) ~ "deb",
    nzchar(Sys.which("rpm"))  ~ "rpm",
    ~ NULL
  )

  if (is.null(type))
    return(NULL)

  # figure out which system packages are required
  sysdeps <- map(sysreqs, renv_sysreqs_resolve)
  syspkgs <- map(sysdeps, `[[`, "packages")

  # collect list of all packages discovered
  allsyspkgs <- sort(unique(unlist(syspkgs, use.names = FALSE)))

  # some packages might be virtual packages, and won't be reported as installed
  # when queried. try to resolve those to the actual underlying packages.
  # some examples follows:
  #
  #   Fedora 41:     zlib-devel       => zlib-ng-compat-devel
  #   Ubuntu 24.04:  libfreetype6-dev => libfreetype-dev
  #
  aliases <- renv_sysreqs_aliases(type, allsyspkgs)
  resolvedpkgs <- alias(allsyspkgs, aliases)

  # list all currently-installed packages
  installedpkgs <- case(
    type == "deb" ~ system("dpkg-query -W -f '${Package}\n'", intern = TRUE),
    type == "rpm" ~ system("rpm --query --all --queryformat='%{Name}\n'", intern = TRUE)
  )

  # check for matches
  misspkgs <- setdiff(resolvedpkgs, installedpkgs)
  if (empty(misspkgs))
    return(TRUE)

  # notify the user
  preamble <- "The following required system packages are not installed:"
  postamble <- "The R packages depending on these system packages may fail to install."
  parts <- map(misspkgs, function(misspkg) {
    needs <- map_lgl(syspkgs, function(syspkg) misspkg %in% syspkg)
    list(misspkg, names(syspkgs)[needs])
  })

  lhs <- extract_chr(parts, 1L)
  rhs <- map_chr(extract(parts, 2L), paste, collapse = ", ")
  messages <- sprintf("%s  [required by %s]", format(lhs), rhs)
  bulletin(preamble, messages, postamble)

  installer <- case(
    nzchar(Sys.which("apt"))    ~ "apt install",
    nzchar(Sys.which("dnf"))    ~ "dnf install",
    nzchar(Sys.which("pacman")) ~ "pacman -S",
    nzchar(Sys.which("yum"))    ~ "yum install",
    nzchar(Sys.which("zypper")) ~ "zypper install",
  )

  preamble <- "An administrator can install these packages with:"
  command <- paste("sudo", installer, paste(misspkgs, collapse = " "))
  bulletin(preamble, command)

  cancel_if(prompt && !proceed())

}

renv_sysreqs_installer <- function(distro) {
  case(
    distro == "debian" ~ "apt install",
    distro == "redhat" ~ "dnf install",
    distro == "ubuntu" ~ "apt install",
    ~ "<install>"
  )
}

renv_sysreqs_update <- function() {

  # save path to sysreqs folder
  dest <- renv_path_normalize("inst/sysreqs/sysreqs.json")

  # move to temporary directory
  renv_scope_tempdir()

  # clone the system requirements repository
  args <- c("clone", "--depth", "1", "https://github.com/rstudio/r-system-requirements")
  renv_system_exec("git", args, action = "cloing rstudio/r-system-requirements")

  # read all of the rules from the requirements repository
  files <- list.files(
    path = "r-system-requirements/rules",
    pattern = "[.]json$",
    full.names = TRUE
  )

  contents <- map(files, renv_json_read)

  # give names without extensions for these files
  names <- basename(files)
  idx <- map_int(gregexpr(".", names, fixed = TRUE), tail, n = 1L)
  names(contents) <- substr(names, 1L, idx - 1L)

  # write to sysreqs.json
  renv_json_write(contents, file = dest)

}
