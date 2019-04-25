
# returns TRUE if problems detected
renv_preflight <- function(lockfile) {

  problems <- stack()

  # check that we can compile C programs
  renv_preflight_compiler(problems)

  # if rJava is being used, ensure that Java is properly configured
  renv_preflight_java(lockfile, problems)

  data <- problems$data()
  if (length(data)) {

    feedback <- lines(
      "The following problems were detected in your environment:",
      "",
      paste(data, collapse = "\n\n"),
      "",
      "The environment may not be restored correctly."
    )

    vwritef(feedback)

  }

  length(data) == 0

}

renv_preflight_compiler <- function(problems) {

  # try to compile a simple program
  program <- "void test() {}"
  file <- renv_tempfile("renv-test-compile-", fileext = ".c")
  writeLines(program, con = file)

  args <- c("CMD", "SHLIB", shQuote(file))
  status <- system2(R(), args, stdout = FALSE, stderr = FALSE)

  if (!identical(status, 0L)) {

    feedback <- lines(
      "- Cannot compile C / C++ files from source.",
      "  Please ensure you have a compiler toolchain installed."
    )

    problems$push(feedback)

  }

}

renv_preflight_java <- function(lockfile, problems) {

  # no need to check if we're not using rJava
  records <- renv_records(lockfile)
  if (is.null(records[["rJava"]]))
    return(TRUE)

  # TODO: no need to do anything if we're only installing binaries?
  switch(
    Sys.info()[["sysname"]],
    Windows = renv_preflight_java_windows(problems),
    renv_preflight_java_unix(problems)
  )

}

renv_preflight_java_windows <- function(problems) {

  home <- Sys.getenv("JAVA_HOME", unset = NA)
  feedback <- case(

    is.na(home) ~ lines(
      "- JAVA_HOME is not set.",
      "  Please ensure you have a Java Development Kit (JDK) installed."
    ),

    !file.exists(home) ~ lines(
      "- JAVA_HOME is set to a non-existent directory.",
      "  Please ensure you have a Java Development Kit (JDK) installed."
    )

  )

  if (!is.null(feedback))
    problems$push(feedback)

}

renv_preflight_java_unix <- function(problems) {

  args <- c("CMD", "javareconf", "--dry-run")
  status <- system2(R(), args, stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L)) {

    feedback <- lines(
      "- Cannot compile Java files from source.",
      "  Please ensure you have a Java Development Kit (JDK) installed."
    )

    problems$push(feedback)

  }

}
