
#' Validate an renv lockfile against a schema
#'
#' @description
#' `renv::lockfile_validate()` can be used to validate your `renv.lock`
#' against a default or custom schema. It can be used to automate checks,
#' check for obvious errors, and ensure that any custom fields you add fit
#' your specific needs.
#'
#' @details
#' See the [JSON Schema docs](https://json-schema.org/) for more information
#' on JSON schemas, their use in validation, and how to write your own schema.
#'
#' `renv::lockfile_validate()` wraps ROpenSci's
#' [`jsonvalidate`](https://docs.ropensci.org/jsonvalidate/) package, passing
#' many of its parameters to that package's `json_validate()` function. Use
#' `?jsonvalidate::json_validate` for more information.
#'
#' @inheritParams renv-params
#'
#' @param lockfile Contents of the lockfile, or a filename containing one.
#'   If not provided, it defaults to the project's lockfile.
#'
#' @param schema Contents of a renv schema, or a filename containing a schema.
#'   If not provided, renv's default schema is used.
#'
#' @param greedy Boolean. Continue after first error?
#'
#' @param error Boolean. Throw an error on parse failure?
#'
#' @param verbose Boolean. If `TRUE`, then an attribute `errors` will list
#'   validation failures as a `data.frame`.
#'
#' @param strict Boolean. Set whether the schema should be parsed strictly or
#'   not. If in strict mode schemas will error to "prevent any unexpected
#'   behaviours or silently ignored mistakes in user schema". For example it
#'   will error if encounters unknown formats or unknown keywords. See
#'   https://ajv.js.org/strict-mode.html for details.
#'
#' @return Boolean. `TRUE` if validation passes. `FALSE` if validation fails.
#'
#' @examples
#' \dontrun{
#'
#' # validate the project's lockfile
#' renv::lockfile_validate()
#'
#' # validate the project's lockfile using a non-default schema
#' renv::lockfile_validate(schema = "/path/to/your/custom/schema.json")
#'
#' # validate a lockfile using its path
#' renv::lockfile_validate(lockfile = "/path/to/your/renv.lock")
#' }
#'
#' @keywords internal
#' @export
lockfile_validate <- function(project = NULL,
                              lockfile = NULL, # Use default project lockfile if not provided
                              schema = NULL, # Use default schema if not provided
                              greedy = FALSE,
                              error = FALSE,
                              verbose = FALSE,
                              strict = FALSE)
{

  project <- renv_project_resolve(project)
  lockfile <- lockfile %||% renv_lockfile_path(project = project)
  schema <- schema %||% system.file("schema",
                                    "draft-07.renv.lock.schema.json",
                                    package = "renv",
                                    mustWork = TRUE)

  # "ajv" engine required for schema specifications later than draft-04
  jsonvalidate::json_validate(lockfile,
                              schema,
                              engine = "ajv",
                              greedy = greedy,
                              error = error,
                              verbose = verbose,
                              strict = strict)
}
