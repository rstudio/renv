# Path for storing global state

By default, renv stores global state in the following OS-specific
folders:

|              |                                           |
|--------------|-------------------------------------------|
| **Platform** | **Location**                              |
| Linux        | `~/.cache/R/renv`                         |
| macOS        | `~/Library/Caches/org.R-project.R/R/renv` |
| Windows      | `%LOCALAPPDATA%/R/cache/R/renv`           |

If desired, this path can be customized by setting the `RENV_PATHS_ROOT`
environment variable. This can be useful if you'd like, for example,
multiple users to be able to share a single global cache.

## Usage

``` r
paths
```

## Customising individual paths

The various state sub-directories can also be individually adjusted, if
so desired (e.g. you'd prefer to keep the cache of package installations
on a separate volume). The various environment variables that can be set
are enumerated below:

|                              |                                                                                                         |
|------------------------------|---------------------------------------------------------------------------------------------------------|
| **Environment Variable**     | **Description**                                                                                         |
| `RENV_PATHS_ROOT`            | The root path used for global state storage.                                                            |
| `RENV_PATHS_LIBRARY`         | The path to the project library.                                                                        |
| `RENV_PATHS_LIBRARY_ROOT`    | The parent path for project libraries.                                                                  |
| `RENV_PATHS_LIBRARY_STAGING` | The parent path used for staged package installs.                                                       |
| `RENV_PATHS_SANDBOX`         | The path to the sandboxed R system library.                                                             |
| `RENV_PATHS_LOCKFILE`        | The path to the [lockfile](https://rstudio.github.io/renv/dev/reference/lockfile-api.md).               |
| `RENV_PATHS_CELLAR`          | The path to the cellar, containing local package binaries and sources.                                  |
| `RENV_PATHS_SOURCE`          | The path containing downloaded package sources.                                                         |
| `RENV_PATHS_BINARY`          | The path containing downloaded package binaries.                                                        |
| `RENV_PATHS_CACHE`           | The path containing cached package installations.                                                       |
| `RENV_PATHS_PREFIX`          | An optional prefix to prepend to the constructed library / cache paths.                                 |
| `RENV_PATHS_RENV`            | The path to the project's renv folder. For advanced users only.                                         |
| `RENV_PATHS_RTOOLS`          | (Windows only) The path to [Rtools](https://cran.r-project.org/bin/windows/Rtools/).                    |
| `RENV_PATHS_EXTSOFT`         | (Windows only) The path containing external software needed for compilation of Windows source packages. |

(If you want these settings to persist in your project, it is
recommended that you add these to an appropriate R startup file. For
example, these could be set in: a project-local `.Renviron`, the
user-level `.Renviron`, or a site-wide file at
`file.path(R.home("etc"), "Renviron.site")`. See
[Startup](https://rdrr.io/r/base/Startup.html) for more details).

Note that renv will append platform-specific and version-specific
entries to the set paths as appropriate. For example, if you have set:

    Sys.setenv(RENV_PATHS_CACHE = "/mnt/shared/renv/cache")

then the directory used for the cache will still depend on the renv
cache version (e.g. `v2`), the R version (e.g. `3.5`) and the platform
(e.g. `x86_64-pc-linux-gnu`). For example:

    /mnt/shared/renv/cache/v2/R-3.5/x86_64-pc-linux-gnu

This ensures that you can set a single `RENV_PATHS_CACHE` environment
variable globally without worry that it may cause collisions or errors
if multiple versions of R needed to interact with the same cache.

If reproducibility of a project is desired on a particular machine, it
is highly recommended that the renv cache of installed packages + binary
packages is backed up and persisted, so that packages can be easily
restored in the future – installation of packages from source can often
be arduous.

## Sharing state across operating systems

If you need to share the same cache with multiple different Linux
operating systems, you may want to set the `RENV_PATHS_PREFIX`
environment variable to help disambiguate the paths used on Linux. For
example, setting `RENV_PATHS_PREFIX = "ubuntu-bionic"` would instruct
renv to construct a cache path like:

    /mnt/shared/renv/cache/v2/ubuntu-bionic/R-3.5/x86_64-pc-linux-gnu

If this is required, it's strongly recommended that this environment
variable is set in your R installation's `Renviron.site` file, typically
located at `file.path(R.home("etc"), "Renviron.site")`, so that it can
be active for any R sessions launched on that machine.

Starting from `renv 0.13.0`, you can also instruct renv to auto-generate
an OS-specific component to include as part of library and cache paths,
by setting the environment variable:

    RENV_PATHS_PREFIX_AUTO = TRUE

The prefix will be constructed based on fields within the system's
`/etc/os-release` file. Note that this is the default behavior with
`renv 1.0.6` when using R 4.4.0 or later.

## Package cellar

If your project depends on one or more R packages that are not available
in any remote location, you can still provide a locally-available
tarball for renv to use during restore. By default, these packages
should be made available in the folder as specified by the
`RENV_PATHS_CELLAR` environment variable. The package sources should be
placed in a file at one of these locations:

- `${RENV_PATHS_CELLAR}/<package>_<version>.<ext>`

- `${RENV_PATHS_CELLAR}/<package>/<package>_<version>.<ext>`

- `<project>/renv/cellar/<package>_<version>.<ext>`

- `<project>/renv/cellar/<package>/<package>_<version>.<ext>`

where `.<ext>` is `.tar.gz` for source packages, or `.tgz` for binaries
on macOS and `.zip` for binaries on Windows. During
[`restore()`](https://rstudio.github.io/renv/dev/reference/restore.md),
renv will search the cellar for a compatible package, and prefer
installation with that copy of the package if appropriate.

## Older versions

Older version of renv used a different default cache location. Those
cache locations are:

|              |                                      |
|--------------|--------------------------------------|
| **Platform** | **Location**                         |
| Linux        | `~/.local/share/renv`                |
| macOS        | `~/Library/Application Support/renv` |
| Windows      | `%LOCALAPPDATA%/renv`                |

If an renv root directory has already been created in one of the old
locations, that will still be used. This change was made to comply with
the CRAN policy requirements of R packages.

## Examples

``` r
# get the path to the project library
path <- renv::paths$library()
```
