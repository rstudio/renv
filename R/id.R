
renv_id_path <- function(project) {
  file.path(project, "renv/project-id")
}

renv_id_generate <- function() {

  methods <- list(
    renv_id_generate_kernel,
    renv_id_generate_uuidgen,
    renv_id_generate_windows
  )

  for (method in methods) {
    id <- catch(method())
    if (is.character(id) && length(id) == 1 && nzchar(id)) {
      id <- toupper(id)
      return(id)
    }
  }

  stop("could not generate uuid for this system")

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

renv_id_generate_windows <- function() {

  if (!renv_platform_windows()) {
    fmt <- "this method is only available on Windows"
    stopf(fmt)
  }

  if (nzchar(Sys.which("cscript"))) {

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

    writeLines(script, con = "uuid.vbs")
    args <- c("//NoLogo", "uuid.vbs")
    id <- renv_system_exec("cscript.exe", args, "generating UUID")
    id <- gsub("(?:^\\{|\\}$)", "", id)
    return(id)

  }

  if (nzchar(Sys.which("powershell"))) {
    command <- "[guid]::NewGuid().ToString()"
    args <- c("-Command", shQuote(command))
    id <- renv_system_exec("powershell.exe", args, "generating UUID")
    return(id)
  }

}
