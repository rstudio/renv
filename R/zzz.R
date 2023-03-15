
.onLoad <- function(libname, pkgname) {
  renv_zzz_load()
}

.onAttach <- function(libname, pkgname) {
  renv_zzz_attach()
}

.onUnload <- function(libpath) {
  renv_task_unload()
}

renv_zzz_load <- function() {

  # registerS3method("[<-", "__renv_dotty__", dotty, envir = .BaseNamespaceEnv)

  renv_metadata_init()
  renv_platform_init()
  renv_binding_init()
  renv_virtualization_init()
  renv_envvars_init()
  renv_log_init()
  renv_methods_init()
  renv_libpaths_init()
  renv_patch_init()
  renv_lock_init()
  renv_sandbox_init()

  if (!renv_metadata_embedded()) {

    # TODO: It's not clear if these callbacks are safe to use when renv is
    # embedded, but it's unlikely that clients would want them anyhow.
    renv_task_create(renv_sandbox_task)
    renv_task_create(renv_snapshot_task)

    # pkgload likes to lock all bindings in a package, even if we might
    # try to unlock those in .onLoad(). Sneak around that.
    setHook(
      packageEvent("renv", "onLoad"),
      renv_binding_init,
    )

  }

  # if an renv project already appears to be loaded, then re-activate
  # the sandbox now -- this is primarily done to support suspend and
  # resume with RStudio where the user profile might not be run
  if (renv_rstudio_available()) {
    project <- getOption("renv.project.path")
    if (!is.null(project))
      renv_sandbox_activate(project = project)
  }

}

renv_zzz_attach <- function() {
  renv_rstudio_fixup()
  renv_exports_attach()
}

renv_zzz_run <- function() {

  # check if we're running devtools::document()
  documenting <- FALSE
  document <- parse(text = "devtools::document")[[1]]
  for (call in sys.calls()) {
    if (identical(call[[1]], document)) {
      documenting <- TRUE
      break
    }
  }

  # if so, then create some files
  if (documenting) {
    renv_zzz_bootstrap()
    renv_zzz_docs()
  }

  # check if we're running as part of R CMD build
  # if so, build our local repository with a copy of ourselves
  building <-
    !is.na(Sys.getenv("R_CMD", unset = NA)) &&
    grepl("Rbuild", basename(dirname(getwd())))

  if (building) {
    renv_zzz_repos()
  }

}

renv_zzz_bootstrap <- function() {

  source <- "templates/template-activate.R"
  target <- "inst/resources/activate.R"

  # read the necessary bootstrap scripts
  scripts <- c("R/bootstrap.R", "R/json-read.R")
  contents <- map(scripts, readLines)
  bootstrap <- unlist(contents)

  # format nicely for insertion
  bootstrap <- paste(" ", bootstrap)
  bootstrap <- paste(bootstrap, collapse = "\n")

  # replace template with bootstrap code
  template <- renv_file_read(source)
  replaced <- renv_template_replace(template, list(BOOTSTRAP = bootstrap))

  # write to resources
  printf("* Generating 'inst/resources/activate.R' ... ")
  writeLines(replaced, con = target)
  writef("Done!")

}

renv_zzz_docs <- function() {

  reg.finalizer(globalenv(), function(object) {

    printf("* Copying vignettes to 'inst/doc' ... ")

    ensure_directory("inst/doc")

    files <- list.files(
      path = "vignettes",
      pattern = "[.](?:R|Rmd|html)$",
    )

    src <- file.path("vignettes", files)
    tgt <- file.path("inst/doc", files)
    file.copy(src, tgt, overwrite = TRUE)

    writef("Done!")

  }, onexit = TRUE)

}

renv_zzz_repos <- function() {

  # don't run if we're running tests
  if (renv_package_checking())
    return()

  # prevent recursion
  installing <- Sys.getenv("RENV_INSTALLING_REPOS", unset = NA)
  if (!is.na(installing))
    return()

  Sys.setenv("RENV_INSTALLING_REPOS" = "TRUE")

  writeLines("** installing renv to package-local repository")

  # get package directory
  pkgdir <- getwd()

  # move to build directory
  tdir <- tempfile("renv-build-")
  ensure_directory(tdir)
  owd <- setwd(tdir)
  on.exit(setwd(owd), add = TRUE)

  # build renv again
  r_cmd_build("renv", path = pkgdir)

  # copy built tarball to inst folder
  src <- list.files(tdir, full.names = TRUE)
  tgt <- file.path(pkgdir, "inst/repos/src/contrib")

  ensure_directory(tgt)
  file.copy(src, tgt)

  # write PACKAGES
  renv_scope_envvars(R_DEFAULT_SERIALIZE_VERSION = "2")
  tools::write_PACKAGES(tgt, type = "source")

}

if (identical(.packageName, "renv")) {
  renv_zzz_run()
}

# if renv is being embedded in another package, make sure we
# run our load / attach hooks so internal state is initialized
if (!identical(.packageName, "renv")) {
  renv_zzz_load()
  renv_zzz_attach()
}
