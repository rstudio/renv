
renv_ci_dependencies <- function() {

  # ensure ci directory exists
  ensure_directory("ci")

  # save R version
  saveRDS(R.version, file = "ci/version.rds", version = 2L)

  # find the packages used in this project
  records <- renv_project_remotes(project = getwd())
  packages <- extract_chr(records, "Package")

  # find recursive dependencies of these packages
  revdeps <- tools::package_dependencies(packages, recursive = TRUE)

  # collapse into unique list
  all <- sort(unique(unlist(revdeps)))

  # get versions of these dependencies
  db <- as.data.frame(
    available.packages(filters = c("OS_type", "duplicates")),
    stringsAsFactors = FALSE
  )

  # retrieve fields
  resolved <- db[db$Package %in% all, c("Package", "Version")]
  rownames(resolved) <- NULL

  # set version if available
  envvar <- case(
    renv_platform_linux()   ~ "RENV_CI_CACHE_VERSION_LINUX",
    renv_platform_macos()   ~ "RENV_CI_CACHE_VERSION_MACOS",
    renv_platform_windows() ~ "RENV_CI_CACHE_VERSION_WINDOWS"
  )

  # set cache version
  version <- Sys.getenv(envvar, unset = NA)
  if (!is.na(version))
    attr(resolved, "cache") <- version

  # print it out, just for our own information
  print(resolved)

  # save to file for hashing
  saveRDS(resolved, file = "ci/dependencies.rds", version = 2L)

}

renv_ci_repair <- function() {

  # get installed packages
  library <- renv_libpaths_default()
  db <- renv_installed_packages(lib.loc = library)

  # attempt to load these packages
  packages <- db$Package
  names(packages) <- packages
  ok <- map_lgl(packages, requireNamespace, quietly = TRUE)

  # check which failed to load
  broken <- packages[!ok]
  if (empty(broken)) {
    vwritef("* All installed packages can be successfully loaded.")
    return(broken)
  }

  # notify the user
  renv_pretty_print(
    values    = paste("-", packages),
    preamble  = "The following package(s) could not be successfully loaded:",
    postamble = "These packages will be removed and later reinstalled.",
    wrap      = FALSE
  )

  # remove each broken package
  paths <- file.path(library, broken)
  unlink(paths, recursive = TRUE)

  return(broken)

}
