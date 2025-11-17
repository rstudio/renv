# Upgrade renv

Upgrade the version of renv associated with a project, including using a
development version from GitHub. Automatically snapshots the updated
renv, updates the activate script, and restarts R.

If you want to update all packages (including renv) to their latest CRAN
versions, use
[`update()`](https://rstudio.github.io/renv/dev/reference/update.md).

## Usage

``` r
upgrade(project = NULL, version = NULL, reload = NULL, prompt = interactive())
```

## Arguments

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

- version:

  The version of renv to be installed.

  When `NULL` (the default), the latest version of renv will be
  installed as available from CRAN (or whatever active package
  repositories are active) Alternatively, you can install the latest
  development version with `"main"`, or a specific commit with a SHA,
  e.g. `"5049cef8a"`.

- reload:

  Boolean; reload renv after install? When `NULL` (the default), renv
  will be re-loaded only if updating renv for the active project. Since
  it's not possible to guarantee a clean reload in the current session,
  this will attempt to restart your R session.

- prompt:

  Boolean; prompt upgrade before proceeding?

## Value

A boolean value, indicating whether the requested version of renv was
successfully installed. Note that this function is normally called for
its side effects.

## Note

`upgrade()` is expected to work for renv versions \>= 1.0.1. To upgrade
from prior versions of renv, users should

[`renv::deactivate();`](https://rstudio.github.io/renv/dev/reference/activate.md)
`install.packages("renv");`
[`renv::activate();`](https://rstudio.github.io/renv/dev/reference/activate.md)
`renv::record("renv")`

## Examples

``` r
if (FALSE) { # \dontrun{

# upgrade to the latest version of renv
renv::upgrade()

# upgrade to the latest version of renv on GitHub (development version)
renv::upgrade(version = "main")

} # }
```
