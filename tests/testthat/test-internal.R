
context("Internal")

test_that("R files have balanced covr exclusions", {

  owd <- setwd("../..")
  on.exit(setwd(owd), add = TRUE)

  errors <- stack()

  files <- list.files("R", full.names = TRUE)
  lapply(files, function(file) {

    nocov <- FALSE
    contents <- readLines(file)

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
