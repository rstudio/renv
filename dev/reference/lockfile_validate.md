# Validate an renv lockfile against a schema

`renv::lockfile_validate()` can be used to validate your `renv.lock`
against a default or custom schema. It can be used to automate checks,
check for obvious errors, and ensure that any custom fields you add fit
your specific needs.

## Usage

``` r
lockfile_validate(
  project = NULL,
  lockfile = NULL,
  schema = NULL,
  greedy = FALSE,
  error = FALSE,
  verbose = FALSE,
  strict = FALSE
)
```

## Arguments

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

- lockfile:

  Contents of the lockfile, or a filename containing one. If not
  provided, it defaults to the project's lockfile.

- schema:

  Contents of a renv schema, or a filename containing a schema. If not
  provided, renv's default schema is used.

- greedy:

  Boolean. Continue after first error?

- error:

  Boolean. Throw an error on parse failure?

- verbose:

  Boolean. If `TRUE`, then an attribute `errors` will list validation
  failures as a `data.frame`.

- strict:

  Boolean. Set whether the schema should be parsed strictly or not. If
  in strict mode schemas will error to "prevent any unexpected
  behaviours or silently ignored mistakes in user schema". For example
  it will error if encounters unknown formats or unknown keywords. See
  https://ajv.js.org/strict-mode.html for details.

## Value

Boolean. `TRUE` if validation passes. `FALSE` if validation fails.

## Details

See the [JSON Schema docs](https://json-schema.org/) for more
information on JSON schemas, their use in validation, and how to write
your own schema.

`renv::lockfile_validate()` wraps ROpenSci's
[`jsonvalidate`](https://docs.ropensci.org/jsonvalidate/) package,
passing many of its parameters to that package's `json_validate()`
function. Use
[`?jsonvalidate::json_validate`](https://docs.ropensci.org/jsonvalidate/reference/json_validate.html)
for more information.

## Examples

``` r
if (FALSE) { # \dontrun{

# validate the project's lockfile
renv::lockfile_validate()

# validate the project's lockfile using a non-default schema
renv::lockfile_validate(schema = "/path/to/your/custom/schema.json")

# validate a lockfile using its path
renv::lockfile_validate(lockfile = "/path/to/your/renv.lock")
} # }
```
