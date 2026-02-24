
test_that("credentials are stripped from URLs", {

  # user:password style
  expect_equal(
    renv_url_sanitize("https://user:token@example.com/repo"),
    "https://example.com/repo"
  )

  # token-only style
  expect_equal(
    renv_url_sanitize("https://token@example.com/repo"),
    "https://example.com/repo"
  )

  # no credentials -- unchanged
  expect_equal(
    renv_url_sanitize("https://example.com/repo"),
    "https://example.com/repo"
  )

  # plain repository name -- unchanged
  expect_equal(
    renv_url_sanitize("CRAN"),
    "CRAN"
  )

})

test_that("a variety of URLs can be parsed", {

  url <- "https://packagemanager.posit.co/cran/latest"
  parts <- renv_url_parse(url)

  expect_equal(renv_url_parse(url), list(
    url = url,
    protocol = "https://",
    domain = "packagemanager.posit.co",
    path = "/cran/latest",
    parameters = named(list()),
    fragment = ""
  ))

  url <- "https://example.com/path/to/page?name=ferret&color=purple"
  parts <- renv_url_parse(url)

  expect_equal(renv_url_parse(url), list(
    url = url,
    protocol = "https://",
    domain = "example.com",
    path = "/path/to/page",
    parameters = list(
      name = "ferret",
      color = "purple"
    ),
    fragment = ""
  ))

})
