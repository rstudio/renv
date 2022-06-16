
# set the access control lists for a directory (recursively), copying the
# ACLs set on a directory 'source' to the directory 'target'
renv_acls_reset <- function(target, source = dirname(target)) {

  getfacl <- Sys.which("getfacl"); setfacl <- Sys.which("setfacl")
  if (nzchar(getfacl) && nzchar(setfacl)) {
    fmt <- "getfacl -p %s | setfacl -R --set-file=- %s"
    command <- sprintf(fmt, renv_shell_path(target), renv_shell_path(source))
    renv_system_exec(command, args = NULL, action = "resetting cache ACLs", quiet = TRUE)
  }

}
