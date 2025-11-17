# Copy packages from user libraries to a project library

`hydrate()` installs missing packages from a user library into the
project library. `hydrate()` is called automatically by
[`init()`](https://rstudio.github.io/renv/dev/reference/init.md), and it
is rare that you should need it otherwise, as it can easily get your
project into an inconsistent state.

It may very occasionally be useful to call `hydrate(update = "all")` if
you want to update project packages to match those installed in your
global library (as opposed to using
[`update()`](https://rstudio.github.io/renv/dev/reference/update.md)
which will get the latest versions from CRAN). In this case, you should
verify that your code continues to work, then call
[`snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md)
to record updated package versions in the lockfile.

## Usage

``` r
hydrate(
  packages = NULL,
  ...,
  library = NULL,
  repos = getOption("repos"),
  update = FALSE,
  sources = NULL,
  prompt = interactive(),
  report = TRUE,
  project = NULL
)
```

## Arguments

- packages:

  The set of R packages to install. When `NULL`, the packages found by
  [`dependencies()`](https://rstudio.github.io/renv/dev/reference/dependencies.md)
  are used.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- library:

  The R library to be hydrated. When `NULL`, the active library as
  reported by [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) is
  used.

- repos:

  The R repositories to be used. If the project depends on any R
  packages which cannot be found within the user library paths, then
  those packages will be installed from these repositories instead.

- update:

  Boolean; should `hydrate()` attempt to update already-installed
  packages if the requested package is already installed in the project
  library? Set this to `"all"` if you'd like *all* packages to be
  refreshed from the source library if possible.

- sources:

  A vector of library paths where renv should look for packages. When
  `NULL` (the default), `hydrate()` will look in the system libraries
  (the user library, the site library and the default library) then the
  renv cache.

  If a package is not found in any of these locations, `hydrate()` will
  try to install it from the active R repositories.

- prompt:

  Boolean; prompt the user before taking any action? Ignored when
  `report = FALSE`.

- report:

  Boolean; display a report of what packages will be installed by
  `renv::hydrate()`?

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

## Value

A named R list, giving the packages that were used for hydration as well
as the set of packages which were not found.

## Examples

``` r
if (FALSE) { # \dontrun{

# hydrate the active library
renv::hydrate()

} # }
```
