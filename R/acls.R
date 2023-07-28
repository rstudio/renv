
renv_acls_reset <- function(source, target = dirname(source)) {

  # only run on Linux for now
  if (!renv_platform_linux())
    return(FALSE)

  # skip if we don't have 'getfacl', 'setfacl'
  getfacl <- Sys.which("getfacl"); setfacl <- Sys.which("setfacl")
  if (!nzchar(getfacl) || !nzchar(setfacl))
    return(FALSE)

  # build command
  fmt <- "getfacl %s 2> /dev/null | setfacl -R --set-file=- %s 2> /dev/null"
  cmd <- sprintf(fmt, renv_shell_path(target), renv_shell_path(source))

  # execute it
  # TODO: Should we report errors? If so, how?
  catch(
    renv_system_exec(
      command = cmd,
      action = "resetting ACLs",
      quiet = TRUE
    )
  )

}
