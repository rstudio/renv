
# a description of the system R is running on: the aspects of the environment
# outside of the R library which can influence the results of an analysis, and
# so which need to be known when reproducing one elsewhere.
#
# for now this is only reported by diagnostics(). whether it should be recorded
# in the lockfile, or in a separate file, is still an open question; a lockfile
# is meant to be portable, and this record is specific to a single machine.
renv_platform_record <- function(project = NULL) {

  project <- renv_project_resolve(project)

  record <- list(
    R              = renv_platform_record_r(),
    OS             = renv_platform_record_os(),
    BLAS           = renv_platform_record_blas(),
    LAPACK         = renv_platform_record_lapack(),
    Software       = renv_platform_record_software(),
    Locale         = renv_platform_record_locale(),
    Rtools         = renv_platform_record_rtools(),
    SystemPackages = renv_platform_record_syspkgs(project)
  )

  reject(record, is.null)

}

renv_platform_record_r <- function() {

  list(
    Version  = format(getRversion()),
    Platform = R.version$platform
  )

}

renv_platform_record_os <- function() {

  info <- the$sysinfo

  record <- list(
    Name        = info$sysname,
    Release     = info$release,
    Version     = info$version,
    Machine     = info$machine,
    Description = renv_platform_record_os_description()
  )

  # on Linux, the distribution matters more than the kernel version
  if (renv_platform_linux()) {
    osrelease <- the$platform
    record$Distro        <- osrelease$ID
    record$DistroVersion <- osrelease$VERSION_ID
    record$Codename      <- osrelease$VERSION_CODENAME
  }

  reject(record, is.null)

}

renv_platform_record_os_description <- function() {

  # utils::osVersion is only available with R (>= 3.5.0)
  utils <- asNamespace("utils")
  if (!exists("osVersion", envir = utils, inherits = FALSE))
    return(NULL)

  get("osVersion", envir = utils, inherits = FALSE)

}

renv_platform_record_blas <- function() {

  software <- catch(extSoftVersion())
  if (inherits(software, "error"))
    return(NULL)

  blas <- software[["BLAS"]]
  if (is.null(blas) || !nzchar(blas))
    return(NULL)

  blas

}

renv_platform_record_lapack <- function() {

  library <- catch(La_library())
  if (inherits(library, "error") || !nzchar(library))
    return(NULL)

  version <- catch(La_version())
  if (inherits(version, "error"))
    version <- NULL

  reject(list(Library = library, Version = version), is.null)

}

renv_platform_record_software <- function() {

  software <- catch(extSoftVersion())
  if (inherits(software, "error"))
    return(NULL)

  # BLAS is reported separately
  software <- software[setdiff(names(software), "BLAS")]
  software <- software[nzchar(software)]
  if (empty(software))
    return(NULL)

  as.list(software)

}

renv_platform_record_locale <- function() {

  list(
    Collate = Sys.getlocale("LC_COLLATE"),
    Ctype   = Sys.getlocale("LC_CTYPE")
  )

}

renv_platform_record_rtools <- function() {

  if (!renv_platform_windows())
    return(NULL)

  spec <- renv_rtools_find()
  if (is.null(spec))
    return(NULL)

  list(
    Version = as.character(spec$version),
    Path    = spec$root
  )

}

# the installed versions of the system packages required by the packages in
# the project lockfile
renv_platform_record_syspkgs <- function(project) {

  if (!renv_platform_linux())
    return(NULL)

  type <- renv_sysreqs_type()
  if (is.null(type))
    return(NULL)

  lockpath <- renv_lockfile_path(project)
  if (!file.exists(lockpath))
    return(NULL)

  lockfile <- renv_lockfile_read(lockpath)
  records <- renv_lockfile_records(lockfile)
  sysreqs <- map(records, `[[`, "SystemRequirements")
  sysdeps <- map(sysreqs, renv_sysreqs_resolve)
  syspkgs <- sort(unique(unlist(map(sysdeps, `[[`, "packages"), use.names = FALSE)))
  if (empty(syspkgs))
    return(NULL)

  # resolve virtual packages to the packages actually installed
  aliases <- renv_sysreqs_aliases(type, syspkgs)
  syspkgs <- alias(syspkgs, aliases)

  versions <- renv_sysreqs_versions(type, syspkgs)
  if (empty(versions))
    return(NULL)

  versions

}
