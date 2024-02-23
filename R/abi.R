
renv_abi_check <- function(packages = NULL,
                           ...,
                           libpaths = NULL,
                           project  = NULL)
{
  if (renv_platform_windows()) {
    writef("- ABI conflict checks are not available on Windows.")
    return()
  }

  # disable via option if necessary
  enabled <- getOption("renv.abi.check", default = TRUE)
  if (identical(enabled, FALSE))
    return()

  # resolve arguments
  project <- renv_project_resolve(project)
  libpaths <- libpaths %||% renv_libpaths_all()

  # read installed packages
  packages <- packages %||% renv_abi_packages(project, libpaths)

  # analyze each package
  problems <- stack()
  map(packages, function(package) {
    tryCatch(
      renv_abi_check_impl(package, problems),
      error = warnify
    )
  })

  # report problems
  data <- problems$data()
  if (empty(data)) {
    fmt <- "- No ABI problems were detected in the set of installed packages."
    writef(fmt)
    return(invisible(data))
  }

  # combine everything together
  tbl <- bind(data)

  # make reports for each different type
  reasons <- unique(tbl$reason)
  if ("Rcpp_precious_list" %in% reasons) {
    packages <- sort(unique(tbl$package[tbl$reason == "Rcpp_precious_list"]))
    caution_bullets(
      "The following packages were built against a newer version of Rcpp than is currently available:",
      packages,
      c(
        paste(
          "These packages depend on Rcpp (>= 1.0.7);",
          "however, Rcpp", renv_package_version("Rcpp"), "is currently installed."
        ),
        "Consider installing a new version of Rcpp with 'install.packages(\"Rcpp\")'."
      )
    )
  }

  if ("missing" %in% reasons) {

    missing <- tbl[tbl$reason == "missing", ]
    caution_bullets(
      "The following required system libraries are unavailable:",
      unique(missing$dependency),
      c(
        "These system libraries may need to be re-installed.",
        "Alternatively, you may need to re-install the packages which depend on these libraries."
      )
    )

    # now, for each dependency, list the packages which require it
    for (dep in unique(missing$dependency)) {
      caution(header(sprintf("%s (required by)", dep)))
      caution(paste("-", sort(tbl$package[missing$dependency == dep])))
      caution()
    }

  }

  invisible(tbl)

}

renv_abi_check_impl <- function(package, problems) {

  # find path to package
  pkgpath <- renv_package_find(package)

  # check for dependency issues
  if (renv_platform_macos())
    renv_abi_deps_macos(package, problems)
  else if (renv_platform_linux())
    renv_abi_deps_linux(package, problems)

  # look for an associated shared object
  shlib <- renv_package_shlib(pkgpath)
  if (!file.exists(shlib))
    return()

  # read symbols from LinkingTo dependency packages
  pkgdesc <- renv_description_read(path = pkgpath)
  if (is.null(pkgdesc$LinkingTo))
    return()

  # read symbols from the library
  symbols <- renv_abi_symbols(shlib)

  # handle Rcpp
  linkdeps <- renv_description_parse_field(pkgdesc$LinkingTo)
  if ("Rcpp" %in% linkdeps$Package)
    renv_abi_check_impl_rcpp(package, symbols, problems)

  # TODO: other checks? more direct symbol checks for other packages?

}

renv_abi_check_impl_rcpp <- function(package, symbols, problems) {

  # read Rcpp symbols
  rcpplib <- renv_package_shlib("Rcpp")
  rcppsyms <- renv_abi_symbols(rcpplib)

  # perform checks for different versions of Rcpp
  renv_abi_check_impl_rcpp_preciouslist(package, symbols, rcppsyms, problems)

}

renv_abi_check_impl_rcpp_preciouslist <- function(package, symbols, rcppsyms, problems) {

  # check for dependency on Rcpp_precious APIs
  required <- grep("Rcpp_precious", symbols$symbol, value = TRUE)
  if (empty(required))
    return()

  # check for Rcpp_precious APIs being available
  available <- grep("Rcpp_precious", rcppsyms$symbol, value = TRUE)
  if (length(available))
    return()

  problem <- renv_abi_problem(
    package    = paste(package, renv_package_version(package)),
    dependency = paste("Rcpp", renv_package_version("Rcpp")),
    reason     = "Rcpp_precious_list"
  )

  problems$push(problem)

}

renv_abi_symbols <- function(path, args = NULL) {

  # invoke nm to read symbols
  output <- renv_system_exec(
    command = "nm",
    args    = c(args, renv_shell_path(path)),
    action  = "reading symbols"
  )

  # parse output
  parts <- strsplit(output, "\\s+")
  data <- .mapply(c, parts, NULL)
  names(data) <- c("offset", "type", "symbol")

  # join into data.frame
  as_data_frame(data)

}

renv_abi_problem <- function(package, dependency, reason) {

  list(
    package    = package,
    dependency = dependency,
    reason     = reason
  )

}

renv_abi_packages <- function(project, libpaths) {

  # create a lockfile
  lockfile <- snapshot(
    library  = libpaths,
    lockfile = NULL,
    type     = "all",
    project  = project
  )

  # return package names
  names(lockfile$Packages)

}

renv_abi_deps_macos <- function(package, problems) {
  # TODO
}

renv_abi_deps_linux <- function(package, problems) {

  # get shlib path, if any
  shlib <- renv_package_shlib(package)
  if (!file.exists(shlib))
    return()

  # attempt to read dependencies
  output <- renv_system_exec("ldd", renv_shell_path(shlib))

  # look for 'not found' entries
  idx <- regexpr(" => not found", output, fixed = TRUE)
  matches <- substring(output, 2L, idx - 1L)

  # drop duplicates, empty strings
  names <- unique(matches[nzchar(matches)])
  if (empty(names))
    return()

  # add problems
  for (name in names) {

    fmt <- "%s %s (%s)"
    package <- sprintf(fmt, package, renv_package_version(package), shlib)

    problem <- renv_abi_problem(
      package    = package,
      dependency = name,
      reason     = "missing"
    )

    problems$push(problem)

  }

}
