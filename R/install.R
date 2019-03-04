
#' Install Packages
#'
#' Install one or more \R packages.
#'
#' `install()` uses the same machinery as [restore()] when installing packages.
#' In particular, this means that the local cache of package installations is
#' used when possible. This helps to avoid re-downloading packages that have
#' already been downloaded before, and re-compiling packages from source when
#' a binary copy of that package is already available.
#'
#' Note that this interface is subject to change -- the goal is to hook into
#' separate package installation backends in the future.
#'
#' @param packages A character vector of \R packages to install. Required
#'   package dependencies (`Depends`, `Imports`, `LinkingTo`) will be installed
#'   as required.
#'
#' @export
install <- function(packages) {

  # create lockfile based on state of R libraries
  lockfile <- renv_lockfile_init()
  lockfile$R$Package <- renv_snapshot_r_packages()

  # initialize restore state
  renv_restore_begin(lockfile, packages)
  on.exit(renv_restore_end(), add = TRUE)

  # retrieve packages
  records <- renv_restore_retrieve(packages, lockfile)

  # if an already-installed package satisfies its known requirements,
  # then no need to install it
  records <- Filter(renv_install_required, records)

  # install from records
  renv_restore_install(records)

}

# NOTE: this routine does a very primitive sort of dependency validation;
# it simply checks if the package is too old and requests an install
# if that's the case. e.g. if pkg A requires pkgB >= 1.1, but pkgB 1.0
# is installed, then this routine marks pkgB as requiring install
renv_install_required <- function(record) {

  state <- renv_restore_state()
  lockfile <- state$lockfile

  # if installation of this package was explicitly requested, keep it
  package <- record$Package
  if (package %in% state$packages)
    return(TRUE)

  # check to see if this package is already installed; if it's not
  # installed then we need to install it
  package <- record$Package
  record <- lockfile$R$Package[[package]]
  if (is.null(record))
    return(TRUE)

  # check and see if the installed version satisfies all requirements
  requirements <- state$requirements[[package]]
  if (is.null(requirements))
    return(FALSE)

  data <- bind_list(requirements$data())
  explicit <- data[nzchar(data$Require) & nzchar(data$Version), ]
  if (nrow(explicit) == 0)
    return(FALSE)

  exprs <- sprintf(
    "numeric_version('%s') %s '%s'",
    record$Version,
    explicit$Require,
    explicit$Version
  )

  expr <- paste(exprs, collapse = " && ")
  satisfied <- catch(eval(parse(text = expr)), envir = baseenv())
  if (inherits(satisfied, "error"))
    warning(satisfied)

  !identical(satisfied, TRUE)

}
