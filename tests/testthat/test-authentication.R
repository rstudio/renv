
context("Authentication")

test_that("renv.auth is respected in various contexts", {

  # using local 'renv.auth.<package>'
  local({
    record <- list(Package = "dplyr")
    renv_scope_options(renv.auth.dplyr = list(GITHUB_PAT = "<pat>"))

    local({
      renv_scope_auth(record = record)
      expect_true(Sys.getenv("GITHUB_PAT") == "<pat>")
    })

    expect_false(Sys.getenv("GITHUB_PAT") == "<pat>")

  })

  # using global 'renv.auth'
  local({
    record <- list(Package = "tidyr")
    renv_scope_options(renv.auth = list(tidyr = list(GITHUB_PAT = "<pat>")))

    local({
      renv_scope_auth(record = record)
      expect_true(Sys.getenv("GITHUB_PAT") == "<pat>")
    })

    expect_false(Sys.getenv("GITHUB_PAT") == "<pat>")
  })

  # using an auth function
  local({

    record <- list(Package = "dplyr")
    renv_scope_options(renv.auth.dplyr = function(record) {
      list(GITHUB_PAT = "<pat>")
    })

    local({
      renv_scope_auth(record = record)
      expect_true(Sys.getenv("GITHUB_PAT") == "<pat>")
    })

    expect_false(Sys.getenv("GITHUB_PAT") == "<pat>")

  })

})

