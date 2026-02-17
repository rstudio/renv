
renv_pipe_create <- function(cmd, open = "r", encoding = "native.enc") {

  # on Windows, pipe() invokes cmd.exe /c <command>; when the command
  # starts with a quoted path, cmd.exe strips the first and last quote
  # characters from the entire line, breaking inner quotes. wrapping the
  # whole command in an extra pair of quotes prevents this.
  if (renv_platform_windows())
    cmd <- paste0('"', cmd, '"')

  pipe(cmd, open = open, encoding = encoding)

}

renv_pipe_read <- function(cmd) {
  con <- renv_pipe_create(cmd)
  defer(close(con))
  readLines(con)
}

renv_pipe_close <- function(con) {
  close(con)
}
