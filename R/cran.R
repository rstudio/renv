
# nocov start

renv_cran_status <- function(email   = NULL,
                             package = NULL,
                             view    = "maintainer")
{
  case(
    view == "maintainer" ~ renv_cran_status_maintainer(email, package),
    TRUE                 ~ stopf("unrecognized view '%s'", view)
  )

}

renv_cran_status_maintainer <- function(email, package) {

  email <- email %||% renv_cran_status_maintainer_email(package = package)
  parts <- strsplit(email, "@", fixed = TRUE)[[1L]]

  fmt <- "https://cran.r-project.org/web/checks/check_results_%s_at_%s.html"
  url <- sprintf(fmt, parts[[1L]], parts[[2L]])

  browseURL(url)

}

renv_cran_status_maintainer_email <- function(package = NULL) {

  mtr <- renv_package_description_field(
    package = package %||% "renv",
    field   = "Maintainer"
  )

  indices <- gregexpr("[<>]", mtr, perl = TRUE)[[1L]]
  substring(mtr, indices[[1L]] + 1L, indices[[2L]] - 1L)

}

renv_cran_mirrors_impl <- function() {

  tryCatch(
    getCRANmirrors(local.only = TRUE),
    error = function(cnd) {
      warning(conditionMessage(cnd))
      NULL
    }
  )

}

renv_cran_mirrors <- function() {

  mirrors <- memoize(
    key = "renv_cran_mirrors",
    value = renv_cran_mirrors_impl()
  )

  c(
    getOption("renv.cran.mirrors", default = character()),
    mirrors$URL,
    "https://cran.rstudio.com/",
    "https://cran.rstudio.org/"
  )

}

# nocov end
