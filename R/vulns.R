
#' Request Vulnerability Information for a Package
#'
#' This function acts as an interface to Posit Package Manager's vulnerability
#' API, making it possible to ascertain if the provided packages have any
#' known vulnerabilities.
#'
#' This function requires the \pkg{curl} package to be installed.
#'
#' @inheritParams renv-params
#'
#' @param packages A vector of package specifications, of the form
#'   `<package>==<version>` or `<package>@<version>`.
#'
#' @param lockfile The path to an `renv` lockfile. When specified, `packages`
#'   is ignored, and vulnerabilities are queried based on the packages defined
#'   in the lockfile.
#'
#' @param repos The Package Manager repository to be queried.
#'
#' @param verbose Boolean; when `TRUE`, verbose information from the `curl`
#'   web request will be printed to the console.
#'
#' @returns An \R list of vulnerability information. Only packages which
#'   have known vulnerabilities will be included in the resulting data object.
#'
#' @export
vulns <- function(packages = NULL,
                  lockfile = NULL,
                  ...,
                  repos = NULL,
                  verbose = FALSE,
                  project = NULL)
{
  if (!requireNamespace("curl", quietly = TRUE))
    stop("renv::vulns() requires the 'curl' package")

  renv_scope_error_handler()
  renv_dots_check(...)
  project <- renv_project_resolve(project)

  packages <- packages %||% {
    lockfile <- lockfile %||% renv_paths_lockfile(project)
    lockfile <- renv_lockfile_read(lockfile)
    records <- renv_lockfile_records(lockfile)
    map_chr(records, function(record) {
      paste(record[["Package"]], record[["Version"]], sep = "==")
    })
  }

  packages <- gsub("@", "==", packages, fixed = TRUE)

  repos <- repos %||% getOption("repos")[[1L]]
  parts <- renv_ppm_parse(repos)
  if (length(parts) == 0L) {
    warningf("failed to parse repository '%s'", repos)
    return(list())
  }

  # begin building a curl handle
  handle <- curl::new_handle(verbose = verbose)

  # set headers for request
  headers <- list("Content-Type" = "application/json")
  curl::handle_setheaders(handle, .list = headers)

  # start building POST options
  data <- list(
    repo                 = parts[["repos"]],
    snapshot             = parts[["snapshot"]],
    names                = as.list(unname(packages)),
    metadata             = TRUE,
    vulns                = TRUE,
    omit_dependencies    = TRUE,
    omit_downloads       = TRUE,
    omit_package_details = TRUE
  )

  json <- renv_json_convert(data)

  # get netrc file path
  curl::handle_setopt(
    handle     = handle,
    post       = TRUE,
    postfields = json
  )

  # use netrc if available
  netrcFile <- getOption("netrc", default = Sys.getenv("NETRC", unset = "~/.netrc"))
  if (file.exists(netrcFile))
  {
    curl::handle_setopt(
      handle     = handle,
      httpauth   = 1L,
      netrc      = 1L,
      netrc_file = path.expand(netrcFile)
    )
  }

  # make the request, collect the response
  endpoint <- file.path(parts[["root"]], "__api__/filter/packages")
  response <- curl::curl_fetch_memory(endpoint, handle = handle)
  contents <- enc2utf8(rawToChar(response$content))
  splat <- strsplit(contents, "\n", fixed = TRUE)[[1L]]
  data <- lapply(splat, function(text) {
    renv_json_read(text = text)
  })

  # handle errors
  for (i in seq_along(data))
  {
    error <- data[[i]][["error"]]
    if (!is.character(error))
      next

    code <- data[[i]][["code"]] %||% "unknown"
    fmt <- "error requesting package metadata; %s [error code %s]"
    msg <- sprintf(fmt, error, as.character(code))
    stop(msg, call. = FALSE)
  }

  data
}
