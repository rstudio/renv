
renv_revdeps_check <- function(project = NULL) {

  project <- renv_project_resolve(project)
  renv_scope_wd(project)
  renv_scope_options(repos = c(renv_bioconductor_repos(project)))

  case(

    startsWith(R.version$platform, "aarch64-apple-darwin") ~ {
      renv_scope_envvars(
        CPPFLAGS = "-I/opt/homebrew/include",
        LDFLAGS  = "-L/opt/homebrew/lib",
        LIBS     = "-L/opt/homebrew/lib"
      )
    }

  )

  package <- renv_description_read("DESCRIPTION", field = "Package")

  renv_infrastructure_write_entry_impl(
    add    = "revdeps",
    remove = character(),
    file   = file.path(project, ".gitignore"),
    create = FALSE
  )

  blueprints <- list(
    list(package = project, root = "revdeps/develop"),
    list(package = package, root = "revdeps/current")
  )

  zmap(blueprints, function(package, root) {
    writef(header("Installing %s [%s]", package, root))
    renv_revdeps_check_preflight(package, "revdeps/develop", project)
    writef()
  })

  revdeps <- package_dependencies(package, reverse = TRUE)[[1L]]
  result <- zmap(blueprints, function(package, root) {
    map(revdeps, function(revdep) {
      writef(header("Checking %s [%s]", revdep, root))
      result <- catch(renv_revdeps_check_impl(revdep, root, project))
      if (inherits(result, "error")) {
        message <- paste("Error:", conditionMessage(result))
        writef(message, con = stderr())
      }
      writef()
      result
    })
  })

}

renv_revdeps_check_preflight <- function(package, root, project) {

  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  renv_scope_wd(root)

  dir.create("library.noindex", showWarnings = FALSE)
  dir.create("results.noindex", showWarnings = FALSE)
  dir.create("sources.noindex", showWarnings = FALSE)
  dir.create("cache.noindex",   showWarnings = FALSE)

  renv_scope_envvars(RENV_PATHS_CACHE = "cache.noindex")
  renv_scope_libpaths("library.noindex")

  install(package, project = project)

  job(function() {
    renv::install("BiocManager")
    renv::install("bioc::BiocVersion")
  })

}

renv_revdeps_check_impl <- function(revdep, root, project) {

  ensure_directory(root)
  root <- normalizePath(root, winslash = "/")
  renv_scope_envvars(RENV_PATHS_SOURCE = file.path(root, "sources.noindex"))
  renv_scope_envvars(RENV_PATHS_CACHE = file.path(root, "cache.noindex"))
  renv_scope_libpaths(file.path(root, "library.noindex"))

  checkpath <- sprintf("%s/results.noindex/%s.Rcheck/00check.log", root, revdep)
  if (file.exists(checkpath)) {
    contents <- readLines(checkpath, warn = FALSE)
    if (startsWith(tail(contents, 1L), "Status:")) {
      writef("- Package was already checked; skipping")
      return()
    }
  }

  record <- renv_remotes_resolve(revdep, latest = TRUE)
  path <- renv_retrieve_path(record)

  job(function() {

    Sys.setenv(RENV_LOG_LEVEL = "error")
    options(renv.cache.linkable = TRUE)
    setwd(!!file.path(root, "results.noindex"))

    result <- install(!!revdep, type = "source", dependencies = "all")
    system2(!!R(), c("CMD", "check", !!path))

  })

}

renv_revdeps_status <- function(packages, root) {

  develop <- map_chr(packages, function(package) {

    checkdir <- sprintf("revdeps/develop/results.noindex/%s.Rcheck", package)

    installfile <- file.path(checkdir, "00install.out")
    if (!file.exists(installfile))
      return(list(failed = TRUE))

    checkfile <- file.path(checkdir, "00check.log")
    if (!file.exists(checkfile))
      return(list(failed = TRUE))

    contents <- readLines(checkfile)
    tail(contents, n = 1L)

  })

}
