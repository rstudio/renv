
renv_sdkroot_init <- function() {

  if (!renv_platform_macos())
    return()

  enabled <- Sys.getenv("RENV_SDKROOT_ENABLED", unset = "TRUE")
  if (!truthy(enabled, default = TRUE))
    return()

  sdkroot <- Sys.getenv("SDKROOT", unset = NA)
  if (!is.na(sdkroot))
    return()

  sdk <- "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk"
  if (!file.exists(sdk))
    return()

  makeconf <- file.path(R.home("etc"), "Makeconf")
  if (!file.exists(makeconf))
    return()

  contents <- readLines(makeconf)
  cxx <- grep("^CXX\\s*=", contents, value = TRUE, perl = TRUE)
  if (length(cxx) == 0L)
    return()

  if (!grepl("(?:/usr/local|/opt/homebrew)/opt/llvm", cxx))
    return()

  Sys.setenv(SDKROOT = sdk)

}
