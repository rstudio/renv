# Project settings

Define project-local settings that can be used to adjust the behavior of
renv with your particular project.

- Get the current value of a setting with (e.g.)
  `settings$snapshot.type()`

- Set current value of a setting with (e.g.)
  `settings$snapshot.type("explicit")`.

Settings are automatically persisted across project sessions by writing
to `renv/settings.json`. You can also edit this file by hand, but you'll
need to restart the session for those changes to take effect.

### `bioconductor.version`

The Bioconductor version to be used with this project. Use this if you'd
like to lock the version of Bioconductor used on a per-project basis.
When unset, renv will try to infer the appropriate Bioconductor release
using the BiocVersion package if installed; if not, renv uses
[`BiocManager::version()`](https://bioconductor.github.io/BiocManager/reference/version.html)
to infer the appropriate Bioconductor version.

### `external.libraries`

A vector of library paths, to be used in addition to the project's own
private library. This can be useful if you have a package available for
use in some system library, but for some reason renv is not able to
install that package (e.g. sources or binaries for that package are not
publicly available, or you have been unable to orchestrate the
pre-requisites for installing some packages from source on your
machine).

### `ignored.packages`

A vector of packages, which should be ignored when attempting to
snapshot the project's private library. Note that if a package has
already been added to the lockfile, that entry in the lockfile will not
be ignored.

### `package.dependency.fields`

When installing a package with
[`install()`](https://rstudio.github.io/renv/dev/reference/install.md),
what `DESCRIPTION` fields should be used to determine that package's
dependencies? The default uses `c("Imports", "Depends", "LinkingTo")`,
but if you also want to install `Suggests` dependencies for a package,
you can set this to `c("Imports", "Depends", "LinkingTo", "Suggests")`.

### `ppm.enabled`

Enable [Posit Package Manager](https://packagemanager.posit.co/)
integration in this project? When `TRUE`, renv will attempt to transform
repository URLs used by PPM into binary URLs as appropriate for the
current Linux platform. Set this to `FALSE` if you'd like to continue
using source-only PPM URLs, or if you find that renv is improperly
transforming your repository URLs. You can still set and use PPM
repositories with this option disabled; it only controls whether renv
tries to transform source repository URLs into binary URLs on your
behalf.

### `ppm.ignored.urls`

When [Posit Package Manager](https://packagemanager.posit.co/)
integration is enabled, `renv` will attempt to transform source
repository URLs into binary repository URLs. This setting can be used if
you'd like to avoid this transformation with some subset of repository
URLs.

### `r.version`

The version of R to encode within the lockfile. This can be set as a
project-specific option if you'd like to allow multiple users to use the
same renv project with different versions of R. renv will still warn the
user if the major + minor version of R used in a project does not match
what is encoded in the lockfile.

### `snapshot.type`

The type of snapshot to perform by default. See
[snapshot](https://rstudio.github.io/renv/dev/reference/snapshot.md) for
more details.

### `snapshot.dev`

Whether to include development dependencies by default when calling
[`renv::snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md)
or
[`renv::status()`](https://rstudio.github.io/renv/dev/reference/status.md).
When `TRUE`, development dependencies (e.g., packages listed in
`Suggests` or development tools like `devtools`) will be included.
Defaults to `FALSE`.

### `use.cache`

Enable the renv package cache with this project. When active, renv will
install packages into a global cache, and link packages from the cache
into your renv projects as appropriate. This can greatly save on disk
space and install time when for R packages which are used across
multiple projects in the same environment.

### `vcs.manage.ignores`

Should renv attempt to manage the version control system's ignore files
(e.g. `.gitignore`) within this project? Set this to `FALSE` if you'd
prefer to take control. Note that if this setting is enabled, you will
need to manually ensure internal data in the project's `renv/` folder is
explicitly ignored.

### `vcs.ignore.cellar`

Set whether packages within a project-local package cellar are excluded
from version control. See
[`vignette("package-sources", package = "renv")`](https://rstudio.github.io/renv/dev/articles/package-sources.md)
for more information.

### `vcs.ignore.library`

Set whether the renv project library is excluded from version control.

### `vcs.ignore.local`

Set whether renv project-specific local sources are excluded from
version control.

## Usage

``` r
settings
```

## Value

A named list of renv settings.

## Defaults

You can change the default values of these settings for newly-created
renv projects by setting R options for `renv.settings` or
`renv.settings.<name>`. For example:

    options(renv.settings = list(snapshot.type = "all"))
    options(renv.settings.snapshot.type = "all")

If both of the `renv.settings` and `renv.settings.<name>` options are
set for a particular key, the option associated with
`renv.settings.<name>` is used instead. We recommend setting these in an
appropriate startup profile, e.g. `~/.Rprofile` or similar.

## Examples

``` r
if (FALSE) { # \dontrun{

# view currently-ignored packaged
renv::settings$ignored.packages()

# ignore a set of packages
renv::settings$ignored.packages("devtools", persist = FALSE)

} # }
```
