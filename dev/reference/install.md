# Install packages

Install one or more R packages, from a variety of remote sources.
`install()` uses the same machinery as
[`restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
(i.e. it uses cached packages where possible) but it does not respect
the lockfile, instead installing the latest versions available from
CRAN.

See
[`vignette("package-install")`](https://rstudio.github.io/renv/dev/articles/package-install.md)
for more details.

## Usage

``` r
install(
  packages = NULL,
  ...,
  include = NULL,
  exclude = NULL,
  library = NULL,
  type = NULL,
  rebuild = FALSE,
  repos = NULL,
  prompt = interactive(),
  dependencies = NULL,
  verbose = NULL,
  transactional = NULL,
  lock = FALSE,
  project = NULL
)
```

## Arguments

- packages:

  Either `NULL` (the default) to install all packages required by the
  project, or a character vector of packages to install. renv supports a
  subset of the remotes syntax used for package installation, e.g:

  - `pkg`: install latest version of `pkg` from CRAN.

  - `pkg@version`: install specified version of `pkg` from CRAN.

  - `username/repo`: install package from GitHub

  - `bioc::pkg`: install `pkg` from Bioconductor.

  See <https://remotes.r-lib.org/articles/dependencies.html> and the
  examples below for more details.

  renv deviates from the remotes spec in one important way:
  subdirectories are separated from the main repository specification
  with a `:`, not `/`. So to install from the `subdir` subdirectory of
  GitHub package `username/repo` you'd use `"username/repo:subdir`.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- include:

  Packages which should be installed. `include` can occasionally be
  useful when you'd like to call `renv::install()` with no arguments,
  but restrict package installation to only some subset of dependencies
  in the project.

- exclude:

  Packages which should not be installed. `exclude` is useful when using
  `renv::install()` to install all dependencies in a project, except for
  a specific set of packages.

- library:

  The R library to be used. When `NULL`, the active project library will
  be used instead.

- type:

  The type of package to install ("source" or "binary"). Defaults to the
  value of `getOption("pkgType")`.

- rebuild:

  Force packages to be rebuilt, thereby bypassing any installed versions
  of the package available in the cache? This can either be a boolean
  (indicating that all installed packages should be rebuilt), or a
  vector of package names indicating which packages should be rebuilt.

- repos:

  The repositories to use when restoring packages installed from CRAN or
  a CRAN-like repository. By default, the repositories recorded in the
  lockfile will be used, ensuring that (e.g.) CRAN packages are
  re-installed from the same CRAN mirror.

  Use `repos = getOption("repos")` to override with the repositories set
  in the current session, or see the `repos.override` option in
  [config](https://rstudio.github.io/renv/dev/reference/config.md) for
  an alternate way override.

- prompt:

  Boolean; prompt the user before taking any action? For backwards
  compatibility, `confirm` is accepted as an alias for `prompt`.

- dependencies:

  A vector of DESCRIPTION field names that should be used for package
  dependency resolution. When `NULL` (the default), the value of
  `renv::settings$package.dependency.fields` is used. The aliases
  "strong", "most", and "all" are also supported. See
  [`tools::package_dependencies()`](https://rdrr.io/r/tools/package_dependencies.html)
  for more details.

- verbose:

  Boolean; report output from `R CMD build` and `R CMD INSTALL` during
  installation? When `NULL` (the default), the value of
  `config$install.verbose()` will be used. When `FALSE`, installation
  output will be emitted only if a package fails to install.

- transactional:

  Whether or not to use a 'transactional' package installation. See
  **Transactional Restore** in
  [`restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
  for more details. When `NULL` (the default), the value of the
  `install.transactional`
  [`config`](https://rstudio.github.io/renv/dev/reference/config.md)
  option will be used.

- lock:

  Boolean; update the `renv.lock` lockfile after the successful
  installation of the requested packages?

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

## Value

A named list of package records which were installed by renv.

## `Remotes`

`install()` (called without arguments) will respect the `Remotes` field
of the `DESCRIPTION` file (if present). This allows you to specify
places to install a package other than the latest version from CRAN. See
<https://remotes.r-lib.org/articles/dependencies.html> for details.

## Bioconductor

Packages from Bioconductor can be installed by using the `bioc::`
prefix. For example,

    renv::install("bioc::Biobase")

will install the latest-available version of Biobase from Bioconductor.

renv depends on BiocManager (or, for older versions of R, BiocInstaller)
for the installation of packages from Bioconductor. If these packages
are not available, renv will attempt to automatically install them
before fulfilling the installation request.

## Examples

``` r
if (FALSE) { # \dontrun{

# install the latest version of 'digest'
renv::install("digest")

# install an old version of 'digest' (using archives)
renv::install("digest@0.6.18")

# install 'digest' from GitHub (latest dev. version)
renv::install("eddelbuettel/digest")

# install a package from GitHub, using specific commit
renv::install("eddelbuettel/digest@df55b00bff33e945246eff2586717452e635032f")

# install a package from Bioconductor
# (note: requires the BiocManager package)
renv::install("bioc::Biobase")

# install a package, specifying path explicitly
renv::install("~/path/to/package")

# install packages as declared in the project DESCRIPTION file
renv::install()

} # }
```
