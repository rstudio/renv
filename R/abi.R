
renv_abi_check <- function(packages = NULL,
                           ...,
                           libpaths = NULL,
                           project  = NULL)
{
  if (renv_platform_windows()) {
    writef("* ABI conflict checks are not yet implemented on Windows.")
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
      error = warning
    )
  })

  # report problmes
  data <- problems$data()
  if (empty(data)) {
    fmt <- "* No ABI conflicts were detected in the set of installed packages."
    writef(fmt)
    return(invisible(data))
  }

  # combine everything together
  tbl <- bind(data)

  # make reports for each different type
  reasons <- unique(tbl$reason)
  if ("Rcpp_precious_list" %in% reasons) {
    packages <- sort(unique(tbl$package[tbl$reason == "Rcpp_precious_list"]))
    renv_pretty_print(
      values    = packages,
      preamble  = "The following packages were built against a newer version of Rcpp than is currently available:",
      postamble = c(
        paste(
          "These packages depend on Rcpp (>= 1.0.7);",
          "however, Rcpp", renv_package_version("Rcpp"), "is currently installed."
        ),
        "Consider installing a new version of Rcpp with 'install.packages(\"Rcpp\")'."
      ),
      wrap = FALSE
    )
  }

  invisible(tbl)

}

renv_abi_check_impl <- function(package, problems) {

  # find path to package
  pkgpath <- renv_package_find(package)

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
    args = c(args, shQuote(path)),
    action = "reading symbols"
  )

  # parse output
  parts <- strsplit(output, "\\s+")
  data <- .mapply(c, parts, NULL)
  names(data) <- c("offset", "type", "symbol")

  # join into data.frame
  as.data.frame(
    data,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

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
