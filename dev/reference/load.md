# Load a project

`renv::load()` sets the library paths to use a project-local library,
sets up the system library
[sandbox](https://rstudio.github.io/renv/dev/reference/sandbox.md), if
needed, and creates shims for
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html),
[`update.packages()`](https://rdrr.io/r/utils/update.packages.html), and
[`remove.packages()`](https://rdrr.io/r/utils/remove.packages.html).

You should not generally need to call `renv::load()` yourself, as it's
called automatically by the project auto-loader created by
[`init()`](https://rstudio.github.io/renv/dev/reference/init.md)/
[`activate()`](https://rstudio.github.io/renv/dev/reference/activate.md).
However, if needed, you can use `renv::load("<project>")` to explicitly
load an renv project located at a particular path.

## Usage

``` r
load(project = NULL, quiet = FALSE, profile = NULL, ...)
```

## Arguments

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

- quiet:

  Boolean; be quiet during load?

- profile:

  The profile to be activated. See
  [`vignette("profiles", package = "renv")`](https://rstudio.github.io/renv/dev/articles/profiles.md)
  for more information. When `NULL` (the default), the profile is not
  changed. Use `profile = "default"` to revert to the default `renv`
  profile.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

## Value

The project directory, invisibly. Note that this function is normally
called for its side effects.

## Shims

To help you take advantage of the package cache, renv places a couple of
shims on the search path:

- [`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
  instead calls
  [`renv::install()`](https://rstudio.github.io/renv/dev/reference/install.md).

- [`remove.packages()`](https://rdrr.io/r/utils/remove.packages.html)
  instead calls
  [`renv::remove()`](https://rstudio.github.io/renv/dev/reference/remove.md).

- [`update.packages()`](https://rdrr.io/r/utils/update.packages.html)
  instead calls
  [`renv::update()`](https://rstudio.github.io/renv/dev/reference/update.md).

This allows you to keep using your existing muscle memory for
installing, updating, and remove packages, while taking advantage of
renv features like the package cache.

If you'd like to bypass these shims within an R session, you can
explicitly call the version of these functions from the utils package,
e.g. with `utils::install.packages(<...>)`.

If you'd prefer not to use the renv shims at all, they can be disabled
by setting the R option `options(renv.config.shims.enabled = FALSE)` or
by setting the environment variable `RENV_CONFIG_SHIMS_ENABLED = FALSE`.
See [`?config`](https://rstudio.github.io/renv/dev/reference/config.md)
for more details.

## Examples

``` r
if (FALSE) { # \dontrun{

# load a project -- note that this is normally done automatically
# by the project's auto-loader, but calling this explicitly to
# load a particular project may be useful in some circumstances
renv::load()

} # }
```
