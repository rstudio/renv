
renv_pipe_create <- function(command, open = "r", encoding = "native.enc") {

  # on Windows, pipe() invokes cmd.exe /c <command>; when the command
  # starts with a quoted path, cmd.exe strips the first and last quote
  # characters from the entire line, breaking inner quotes. wrapping the
  # whole command in an extra pair of quotes prevents this.
  if (renv_platform_windows())
    command <- paste0("\"", command, "\"")

  pipe(command, open = open, encoding = encoding)

}
