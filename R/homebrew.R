
renv_homebrew_root <- function() {

  # allow override
  root <- Sys.getenv("RENV_HOMEBREW_ROOT", unset = NA)
  if (!is.na(root))
    return(root)

  # indirection for arm64 macOS
  if (renv_platform_macos() && renv_platform_machine() != "x86_64")
    return("/opt/homebrew")

  # default to /usr/local
  "/usr/local"

}
