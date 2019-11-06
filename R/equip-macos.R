
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
  specs <- renv_equip_macos_specs()
  majmin <- numeric_version(version)[1, 1:2]
  specs[[format(majmin)]]
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

  spec <- renv_equip_macos_spec()
  if (is.null(spec)) {
    fmt <- "no known toolchain recorded in renv for R %s"
    warningf(fmt, version)
    return(FALSE)
  }

  url <- spec$url
  dst <- spec$dst

  clang <- file.path(dst, "bin/clang")
  if (file.exists(clang)) {
    fmt <- "* LLVM toolchain for R %s is already installed at %s."
    vwritef(fmt, getRversion(), shQuote(dst))
    return(TRUE)
  }

  destfile <- file.path(tempdir(), basename(url))
  download(url, destfile = destfile)

  if (renv_equip_macos_rstudio(spec, destfile))
    return(TRUE)

  command <- paste("sudo /usr/sbin/installer -pkg", shQuote(destfile), "-target /")
  renv_pretty_print(
    command,
    "The R LLVM toolchain has been successfully downloaded. Please execute:",
    "in a separate terminal to complete installation.",
    wrap = FALSE
  )

  TRUE

}

renv_equip_macos_rstudio <- function(spec, destfile) {

  rstudio <-
    .Platform$GUI == "RStudio" &&
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

  renv_pretty_print(
    spec$dst,
    "The R LLVM toolchain has been downloaded and installed to:",
    "This toolchain will be used by renv when installing packages from source.",
    wrap = FALSE
  )

  return(TRUE)

}
