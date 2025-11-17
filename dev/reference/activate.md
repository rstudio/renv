# Activate or deactivate a project

`activate()` enables renv for a project in both the current session and
in all future sessions. You should not generally need to call
`activate()` yourself as it's called automatically by
[`init()`](https://rstudio.github.io/renv/dev/reference/init.md), which
is the best way to start using renv in a new project.

`activate()` first calls
[`scaffold()`](https://rstudio.github.io/renv/dev/reference/scaffold.md)
to set up the project infrastructure. Most importantly, this creates a
project library and adds a an auto-loader to `.Rprofile` to ensure that
the project library is automatically used for all future instances of
the project. It then restarts the session to use that auto-loader.

`deactivate()` removes the infrastructure added by `activate()`, and
restarts the session. By default it will remove the auto-loader from the
`.Rprofile`; use `clean = TRUE` to also delete the lockfile and the
project library.

## Usage

``` r
activate(project = NULL, profile = NULL)

deactivate(project = NULL, clean = FALSE)
```

## Arguments

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

- profile:

  The profile to be activated. See
  [`vignette("profiles", package = "renv")`](https://rstudio.github.io/renv/dev/articles/profiles.md)
  for more information. When `NULL` (the default), the profile is not
  changed. Use `profile = "default"` to revert to the default `renv`
  profile.

- clean:

  If `TRUE`, will also remove the `renv/` directory and the lockfile.

## Value

The project directory, invisibly. Note that this function is normally
called for its side effects.

## Temporary deactivation

If you need to temporarily disable autoload activation you can set the
`RENV_CONFIG_AUTOLOADER_ENABLED` envvar, e.g.
`Sys.setenv(RENV_CONFIG_AUTOLOADER_ENABLED = "false")`.

## Examples

``` r
if (FALSE) { # \dontrun{

# activate the current project
renv::activate()

# activate a separate project
renv::activate(project = "~/projects/analysis")

# deactivate the currently-activated project
renv::deactivate()

} # }
```
