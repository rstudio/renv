
renv_id_path <- function(project) {
  file.path(project, "renv/project-id")
}

renv_id_generate <- function() {

  methods <- list(
    renv_id_generate_r,
    renv_id_generate_kernel,
    renv_id_generate_uuidgen,
    renv_id_generate_cscript,
    renv_id_generate_powershell
  )

  for (method in methods) {
    id <- catch(method())
    if (is.character(id) && length(id) == 1 && nzchar(id)) {
      id <- toupper(id)
      return(id)
    }
  }

  stop("could not generate project id for this system")

}

renv_id_generate_kernel <- function() {

  uuidpath <- "/proc/sys/kernel/random/uuid"
  if (!file.exists(uuidpath)) {
    fmt <- "%s does not exist on this operating system"
    stopf(fmt, renv_path_pretty(uuidpath))
  }

  readLines(uuidpath, n = 1L, warn = FALSE)

}

renv_id_generate_uuidgen <- function() {

  if (!nzchar(Sys.which("uuidgen"))) {
    fmt <- "program %s does not exist on this system"
    stopf(fmt, shQuote("uuidgen"))
  }

  system("uuidgen", intern = TRUE)

}

renv_id_generate_cscript <- function() {

  if (!renv_platform_windows()) {
    fmt <- "this method is only available on Windows"
    stopf(fmt)
  }

  if (!nzchar(Sys.which("cscript.exe"))) {
    fmt <- "could not find cscript.exe"
    stopf(fmt)
  }

  # create temporary directory
  dir <- tempfile("renv-id-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # move to it
  owd <- setwd(dir)
  on.exit(setwd(owd), add = TRUE)

  # write helper script
  script <- c(
    "set object = CreateObject(\"Scriptlet.TypeLib\")",
    "WScript.StdOut.WriteLine object.GUID"
  )

  # invoke it
  writeLines(script, con = "uuid.vbs")
  args <- c("//NoLogo", "uuid.vbs")
  id <- renv_system_exec("cscript.exe", args, "generating UUID")

  # remove braces
  gsub("(?:^\\{|\\}$)", "", id)

}

renv_id_generate_powershell <- function() {

  if (!renv_platform_windows()) {
    fmt <- "this method is only available on Windows"
    stopf(fmt)
  }

  if (!nzchar(Sys.which("powershell.exe"))) {
    fmt <- "could not find powershell.exe"
    stopf(fmt)
  }

  command <- "[guid]::NewGuid().ToString()"
  args <- c("-Command", shQuote(command))
  renv_system_exec("powershell.exe", args, "generating UUID")

}

renv_id_generate_r <- function() {

  if ("uuid" %in% loadedNamespaces())
    return(uuid::UUIDgenerate())

  libpaths <- c(
    .libPaths(),
    renv_libpaths_user(),
    renv_libpaths_site(),
    renv_libpaths_system()
  )

  if (!requireNamespace("uuid", lib.loc = libpaths, quietly = TRUE))
    stop("could not load package 'uuid'")

  id <- uuid::UUIDgenerate()
  catchall(unloadNamespace("uuid"))
  id

}
