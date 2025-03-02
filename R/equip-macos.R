
renv_equip_macos_specs <- function() {

  list(

    "4.0" = list(
      url = "https://cran.r-project.org/bin/macosx/tools/clang-8.0.0.pkg",
      dst = "/usr/local/clang8"
    ),

    "3.7" = list(
      url = "https://cran.r-project.org/bin/macosx/tools/clang-8.0.0.pkg",
      dst = "/usr/local/clang8"
    ),

    "3.6" = list(
      url = "https://cran.r-project.org/bin/macosx/tools/clang-7.0.0.pkg",
      dst = "/usr/local/clang7"
    ),

    "3.5" = list(
      url = "https://cran.r-project.org/bin/macosx/tools/clang-6.0.0.pkg",
      dst = "/usr/local/clang6"
    )

  )

}

renv_equip_macos_spec <- function(version = getRversion()) {
  renv_equip_macos_specs()[[renv_version_maj_min(version)]]
}

renv_equip_macos <- function() {

  renv_equip_macos_sdk()
  renv_equip_macos_toolchain()

}

renv_equip_macos_sdk <- function() {

  sdk <- "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk"
  if (file.exists(sdk) || file.exists("/usr/include"))
    return(TRUE)

  system("/usr/bin/xcode-select --install")

  # give the user some time to respond to the dialog)
  Sys.sleep(5)

}

renv_equip_macos_toolchain <- function() {

  if (getRversion() >= "4.1.0")
    return()

  spec <- renv_equip_macos_spec()
  if (is.null(spec)) {
    fmt <- "no known toolchain recorded in renv for R %s"
    warningf(fmt, getRversion())
    return(FALSE)
  }

  url <- spec$url
  dst <- spec$dst

  clang <- file.path(dst, "bin/clang")
  if (file.exists(clang)) {
    fmt <- "- LLVM toolchain for R %s is already installed at %s."
    writef(fmt, getRversion(), shQuote(dst))
    return(TRUE)
  }

  destfile <- file.path(tempdir(), basename(url))
  download(url, destfile = destfile)

  if (renv_equip_macos_rstudio(spec, destfile))
    return(TRUE)

  command <- paste("sudo /usr/sbin/installer -pkg", shQuote(destfile), "-target /")
  bulletin(
    "The R LLVM toolchain has been successfully downloaded. Please execute:",
    command,
    "in a separate terminal to complete installation."
  )

  TRUE

}

renv_equip_macos_rstudio <- function(spec, destfile) {

  rstudio <-
    renv_rstudio_available() &&
    requireNamespace("rstudioapi", quietly = TRUE)

  if (!rstudio)
    return(FALSE)

  command <- paste("sudo -kS /usr/sbin/installer -pkg", shQuote(destfile), "-target /")
  prompt <- paste(
    "Installation of the R LLVM toolchain requires sudo.",
    "Please enter your account password.",
    sep = "\n"
  )

  installed <- local({

    password <- rstudioapi::askForPassword(prompt)
    if (is.null(password))
      return(FALSE)

    status <- system(command, input = password)
    if (status != 0L)
      return(FALSE)

    TRUE

  })

  if (!installed)
    return(FALSE)

  bulletin(
    "The R LLVM toolchain has been downloaded and installed to:",
    spec$dst,
    "This toolchain will be used by renv when installing packages from source."
  )

  return(TRUE)

}
