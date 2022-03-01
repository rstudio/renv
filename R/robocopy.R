
renv_robocopy_exec <- function(source, target, flags = NULL) {

  source <- path.expand(source)
  target <- path.expand(target)

  # add other flags
  flags <- c(flags, "/E", "/Z", "/R:5", "/W:10")

  # https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/robocopy
  # > Any value greater than 8 indicates that there was at least one failure
  # > during the copy operation.
  renv_system_exec(
    command = "robocopy",
    args    = c(flags, renv_shell_path(source), renv_shell_path(target)),
    action  = "copying directory",
    success = 0:8,
    quiet   = TRUE
  )

}

renv_robocopy_copy <- function(source, target) {
  renv_robocopy_exec(source, target)
}

renv_robocopy_move <- function(source, target) {
  renv_robocopy_exec(source, target, "/MOVE")
}
