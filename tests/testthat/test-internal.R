
test_that("R files have balanced covr exclusions", {

  renv_scope_wd("../..")

  errors <- stack()

  files <- list.files("R", pattern = "[.][rR]$", full.names = TRUE)
  lapply(files, function(file) {

    nocov <- FALSE
    contents <- catch(readLines(file))
    if (inherits(contents, "error")) {
      writef("[%s]: %s", file, conditionMessage(contents))
      return()
    }

    for (i in seq_along(contents)) {
      line <- contents[[i]]

      if (grepl("#\\s+nocov\\s+start\\s*$", line)) {
        if (nocov) {
          errors$push(list(file, i, "# nocov start"))
        } else {
          nocov <- TRUE
        }
      }

      if (grepl("#\\s+nocov\\s+end\\s*$", line)) {
        if (nocov) {
          nocov <- FALSE
        } else {
          errors$push(list(file, i, "# nocov end"))
        }
      }

    }

  })

  invalid <- errors$data()
  if (length(invalid)) {
    lines <- map_chr(invalid, function(loc) {
      fmt <- "[%s:%i]: unexpected '%s'"
      sprintf(fmt, loc[[1]], loc[[2]], loc[[3]])
    })
    warning(lines)
  }

  expect_length(invalid, 0L)

})
