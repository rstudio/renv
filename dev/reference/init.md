# Use renv in a project

Call `renv::init()` to start using renv in the current project. This
will:

1.  Set up project infrastructure (as described in
    [`scaffold()`](https://rstudio.github.io/renv/dev/reference/scaffold.md))
    including the project library and the `.Rprofile` that ensures renv
    will be used in all future sessions,

2.  Discover the packages that are currently being used in your project
    (via
    [`dependencies()`](https://rstudio.github.io/renv/dev/reference/dependencies.md)),
    and install them into the project library (as described in
    [`hydrate()`](https://rstudio.github.io/renv/dev/reference/hydrate.md)),

3.  Create a lockfile that records the state of the project library so
    it can be restored by others (as described in
    [`snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md)),

4.  Restart R (if running inside RStudio).

If you call `renv::init()` with a project that is already using renv, it
will attempt to do the right thing: it will restore the project library
if it's missing, or otherwise ask you what to do.

## Usage

``` r
init(
  project = NULL,
  ...,
  profile = NULL,
  settings = NULL,
  bare = FALSE,
  force = FALSE,
  repos = NULL,
  bioconductor = NULL,
  load = TRUE,
  restart = interactive()
)
```

## Arguments

- project:

  The project directory. When `NULL` (the default), the current working
  directory will be used. The R working directory will be changed to
  match the requested project directory.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- profile:

  The profile to be activated. See
  [`vignette("profiles", package = "renv")`](https://rstudio.github.io/renv/dev/articles/profiles.md)
  for more information. When `NULL` (the default), the profile is not
  changed. Use `profile = "default"` to revert to the default `renv`
  profile.

- settings:

  A list of
  [settings](https://rstudio.github.io/renv/dev/reference/settings.md)
  to be used with the newly-initialized project.

- bare:

  Boolean; initialize the project with an empty project library, without
  attempting to discover and install R package dependencies?

- force:

  Boolean; force initialization? By default, renv will refuse to
  initialize the home directory as a project, to defend against
  accidental misusages of `init()`.

- repos:

  The R repositories to be used in this project. See **Repositories**
  for more details.

- bioconductor:

  The version of Bioconductor to be used with this project. Setting this
  may be appropriate if renv is unable to determine that your project
  depends on a package normally available from Bioconductor. Set this to
  `TRUE` to use the default version of Bioconductor recommended by the
  `BiocManager` package. When `NULL` (the default), the value is
  inferred from the `bioconductor.init` configuration option – see
  [config](https://rstudio.github.io/renv/dev/reference/config.md) for
  more details.

- load:

  Boolean; should the project be loaded after it is initialized?

- restart:

  Boolean; attempt to restart the R session after initializing the
  project? A session restart will be attempted if the `"restart"` R
  option is set by the frontend hosting R.

## Value

The project directory, invisibly. Note that this function is normally
called for its side effects.

## Repositories

If the default R repositories have not already been set, renv will use
the [Posit Public Package Manager](https://packagemanager.posit.co/)
CRAN mirror for package installation. The primary benefit to using this
mirror is that it can provide pre-built binaries for R packages on a
variety of commonly-used Linux distributions. This behavior can be
configured or disabled if desired – see the options in
[`config()`](https://rstudio.github.io/renv/dev/reference/config.md) for
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
