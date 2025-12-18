# Report inconsistencies between lockfile, library, and dependencies

`renv::status()` reports issues caused by inconsistencies across the
project lockfile, library, and
[`dependencies()`](https://rstudio.github.io/renv/dev/reference/dependencies.md).
In general, you should strive to ensure that `status()` reports no
issues, as this maximizes your chances of successfully
[`restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)ing
the project in the future or on another machine.

[`renv::load()`](https://rstudio.github.io/renv/dev/reference/load.md)
will report if any issues are detected when starting an renv project; we
recommend resolving these issues before doing any further work on your
project.

See the headings below for specific advice on resolving any issues
revealed by `status()`.

## Usage

``` r
status(
  project = NULL,
  ...,
  library = NULL,
  lockfile = NULL,
  sources = TRUE,
  cache = FALSE,
  dev = NULL
)
```

## Arguments

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- library:

  The library paths. By default, the library paths associated with the
  requested project are used.

- lockfile:

  Path to a lockfile. When `NULL` (the default), the `renv.lock` located
  in the root of the current project will be used.

- sources:

  Boolean; check that each of the recorded packages have a known
  installation source? If a package has an unknown source, renv may be
  unable to restore it.

- cache:

  Boolean; perform diagnostics on the global package cache? When `TRUE`,
  renv will validate that the packages installed into the cache are
  installed at the expected + proper locations, and validate the hashes
  used for those storage locations.

- dev:

  Boolean; include development dependencies? These packages are
  typically required when developing the project, but not when running
  it (i.e. you want them installed when humans are working on the
  project but not when computers are deploying it).

  Development dependencies include packages listed in the `Suggests`
  field of a `DESCRIPTION` found in the project root, and roxygen2 or
  devtools if their use is implied by other project metadata. They also
  include packages used in `~/.Rprofile` if `config$user.profile()` is
  `TRUE`.

## Value

This function is normally called for its side effects, but it invisibly
returns a list containing the following components:

- `library`: packages in your library.

- `lockfile`: packages in the lockfile.

- `synchronized`: are the library and lockfile in sync?

## Missing packages

`status()` first checks that all packages used by the project are
installed. This must be done first because if any packages are missing
we can't tell for sure that a package isn't used; it might be a
dependency that we don't know about. Once you have resolve any
installation issues, you'll need to run `status()` again to reveal the
next set of potential problems.

There are four possibilities for an uninstalled package:

- If it's used and recorded, call
  [`renv::restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
  to install the version specified in the lockfile.

- If it's used and not recorded, call
  [`renv::install()`](https://rstudio.github.io/renv/dev/reference/install.md)
  to install it from CRAN or elsewhere.

- If it's not used and recorded, call
  [`renv::snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md)
  to remove it from the lockfile.

- If it's not used and not recorded, there's nothing to do. This the
  most common state because you only use a small fraction of all
  available packages in any one project.

If you have multiple packages in an inconsistent state, we recommend
[`renv::restore()`](https://rstudio.github.io/renv/dev/reference/restore.md),
then
[`renv::install()`](https://rstudio.github.io/renv/dev/reference/install.md),
then
[`renv::snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md),
but that also suggests you should be running status more frequently.

## Lockfile vs [`dependencies()`](https://rstudio.github.io/renv/dev/reference/dependencies.md)

Next we need to ensure that packages are recorded in the lockfile if and
only if they are used by the project. Fixing issues of this nature only
requires calling
[`snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md)
because there are four possibilities for a package:

- If it's used and recorded, it's ok.

- If it's used and not recorded, call
  [`renv::snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md)
  to add it to the lockfile.

- If it's not used but is recorded, call
  [`renv::snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md)
  to remove it from the lockfile.

- If it's not used and not recorded, it's also ok, as it may be a
  development dependency.

## Out-of-sync sources

The final issue to resolve is any inconsistencies between the version of
the package recorded in the lockfile and the version installed in your
library. To fix these issues you'll need to either call
[`renv::restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
or
[`renv::snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md):

- Call
  [`renv::snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md)
  if your project code is working. This implies that the library is
  correct and you need to update your lockfile.

- Call
  [`renv::restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
  if your project code isn't working. This probably implies that you
  have the wrong package versions installed and you need to restore from
  known good state in the lockfile.

If you're not sure which case applies, it's generally safer to call
[`renv::snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md).
If you want to rollback to an earlier known good status, see
[`history()`](https://rstudio.github.io/renv/dev/reference/history.md)
and
[`revert()`](https://rstudio.github.io/renv/dev/reference/history.md).

## Different R Version

renv will also notify you if the version of R used when the lockfile was
generated, and the version of R currently in use, do not match. In this
scenario, you'll need to consider:

- Is the version of R recorded in the lockfile correct? If so, you'll
  want to ensure that version of R is installed and used when working in
  this project.

- Otherwise, you can call
  [`renv::snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md)
  to update the version of R recorded in the lockfile, to match the
  version of R currently in use.

If you'd like to set the version of R recorded in a lockfile
independently of the version of R currently in use, you can set the
`r.version` project setting – see
[settings](https://rstudio.github.io/renv/dev/reference/settings.md) for
more details.

## Examples

``` r
if (FALSE) { # \dontrun{

# disable automatic snapshots
auto.snapshot <- getOption("renv.config.auto.snapshot")
options(renv.config.auto.snapshot = FALSE)

# initialize a new project (with an empty R library)
renv::init(bare = TRUE)

# install digest 0.6.19
renv::install("digest@0.6.19")

# save library state to lockfile
renv::snapshot()

# remove digest from library
renv::remove("digest")

# check library status
renv::status()

# restore lockfile, thereby reinstalling digest 0.6.19
renv::restore()

# restore automatic snapshots
options(renv.config.auto.snapshot = auto.snapshot)

} # }
```
