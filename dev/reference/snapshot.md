# Record current state of the project library in the lockfile

Call `renv::snapshot()` to update a
[lockfile](https://rstudio.github.io/renv/dev/reference/lockfile-api.md)
with the current state of dependencies in the project library. The
lockfile can be used to later
[restore](https://rstudio.github.io/renv/dev/reference/restore.md) these
dependencies as required.

It's also possible to call `renv::snapshot()` with a non-renv project,
in which case it will record the current state of dependencies in the
current library paths. This makes it possible to
[restore](https://rstudio.github.io/renv/dev/reference/restore.md) the
current packages, providing lightweight portability and reproducibility
without isolation.

If you want to automatically snapshot after each change, you can set
`config$config$auto.snapshot(TRUE)` – see
[`?config`](https://rstudio.github.io/renv/dev/reference/config.md) for
more details.

## Usage

``` r
snapshot(
  project = NULL,
  ...,
  library = NULL,
  lockfile = paths$lockfile(project = project),
  type = settings$snapshot.type(project = project),
  dev = NULL,
  repos = getOption("repos"),
  packages = NULL,
  exclude = NULL,
  prompt = interactive(),
  update = FALSE,
  force = FALSE,
  reprex = FALSE
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

  The R libraries to snapshot. When `NULL`, the active R libraries (as
  reported by [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)) are
  used.

- lockfile:

  The location where the generated lockfile should be written. By
  default, the lockfile is written to a file called `renv.lock` in the
  project directory. When `NULL`, the lockfile (as an R object) is
  returned directly instead.

- type:

  The type of snapshot to perform:

  - `"implicit"`, (the default), uses all packages captured by
    [`dependencies()`](https://rstudio.github.io/renv/dev/reference/dependencies.md).

  - `"explicit"` uses packages recorded in `DESCRIPTION`.

  - `"all"` uses all packages in the project library.

  - `"custom"` uses a custom filter.

  See **Snapshot types** below for more details.

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

- repos:

  The R repositories to be recorded in the lockfile. Defaults to the
  currently active package repositories, as retrieved by
  `getOption("repos")`.

- packages:

  A vector of packages to be included in the lockfile. When `NULL` (the
  default), all packages relevant for the type of snapshot being
  performed will be included. When set, the `type` argument is ignored.
  Recursive dependencies of the specified packages will be added to the
  lockfile as well.

- exclude:

  A vector of packages to be explicitly excluded from the lockfile. Note
  that transitive package dependencies will always be included, to avoid
  potentially creating an incomplete / non-functional lockfile.

- prompt:

  Boolean; prompt the user before taking any action? For backwards
  compatibility, `confirm` is accepted as an alias for `prompt`.

- update:

  Boolean; if the lockfile already exists, then attempt to update that
  lockfile without removing any prior package records.

- force:

  Boolean; force generation of a lockfile even when pre-flight
  validation checks have failed?

- reprex:

  Boolean; generate output appropriate for embedding the lockfile as
  part of a [reprex](https://tidyverse.org/help/#reprex)?

## Value

The generated lockfile, as an R object (invisibly). Note that this
function is normally called for its side effects.

## Snapshot types

Depending on how you prefer to manage your R package dependencies, you
may want to enable an alternate snapshot type.. The types available are
as follows:

- `"implicit"`:

  (The default) Capture only packages which appear to be used in your
  project, as determined by
  [`renv::dependencies()`](https://rstudio.github.io/renv/dev/reference/dependencies.md).
  This ensures that only the packages actually required by your project
  will enter the lockfile; the downside if it might be slow if your
  project contains a large number of files. If speed becomes an issue,
  you might consider using `.renvignore` files to limit which files renv
  uses for dependency discovery, or switching to explicit mode, as
  described next.

- `"explicit"`:

  Only capture packages which are explicitly listed in the project
  `DESCRIPTION` file. This workflow is recommended for users who wish to
  manage their project's R package dependencies directly, and can be
  used for both package and non-package R projects. Packages used in
  this manner should be recorded in either the `Depends` or `Imports`
  field of the `DESCRIPTION` file.

- `"all"`:

  Capture all packages within the active R libraries in the lockfile.
  This is the quickest and simplest method, but may lead to undesired
  packages (e.g. development dependencies) entering the lockfile.

- `"custom"`:

  Like `"implicit"`, but use a custom user-defined filter instead. The
  filter should be specified by the R option `renv.snapshot.filter`, and
  should either be a character vector naming a function (e.g.
  `"package::method"`), or be a function itself. The function should
  only accept one argument (the project directory), and should return a
  vector of package names to include in the lockfile.

You can change the snapshot type for the current project with
[`settings()`](https://rstudio.github.io/renv/dev/reference/settings.md).
For example, the following code will switch to using `"explicit"`
snapshots:

    renv::settings$snapshot.type("explicit")

When the `packages` argument is set, `type` is ignored, and instead only
the requested set of packages, and their recursive dependencies, will be
written to the lockfile.

## See also

More on handling package
[`dependencies()`](https://rstudio.github.io/renv/dev/reference/dependencies.md)

Other reproducibility:
[`lockfiles`](https://rstudio.github.io/renv/dev/reference/lockfiles.md),
[`restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)

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
