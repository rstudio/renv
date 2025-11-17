# Lockfiles

A **lockfile** records the state of a project at some point in time.

## Usage

``` r
lockfile_create(
  type = settings$snapshot.type(project = project),
  libpaths = .libPaths(),
  packages = NULL,
  exclude = NULL,
  prompt = interactive(),
  force = FALSE,
  ...,
  project = NULL
)

lockfile_read(file = NULL, ..., project = NULL)

lockfile_write(lockfile, file = NULL, ..., project = NULL)

lockfile_modify(
  lockfile = NULL,
  ...,
  remotes = NULL,
  repos = NULL,
  project = NULL
)
```

## Arguments

- type:

  The type of snapshot to perform:

  - `"implicit"`, (the default), uses all packages captured by
    [`dependencies()`](https://rstudio.github.io/renv/dev/reference/dependencies.md).

  - `"explicit"` uses packages recorded in `DESCRIPTION`.

  - `"all"` uses all packages in the project library.

  - `"custom"` uses a custom filter.

  See **Snapshot types** below for more details.

- libpaths:

  The library paths to be used when generating the lockfile.

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

- force:

  Boolean; force generation of a lockfile even when pre-flight
  validation checks have failed?

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

- file:

  A file path, or R connection.

- lockfile:

  An `renv` lockfile; typically created by either `lockfile_create()` or
  `lockfile_read()`.

- remotes:

  An R vector of remote specifications.

- repos:

  A named vector, mapping R repository names to their URLs.

## Details

A lockfile captures the state of a project's library at some point in
time. In particular, the package names, their versions, and their
sources (when known) are recorded in the lockfile.

Projects can be restored from a lockfile using the
[`restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
function. This implies reinstalling packages into the project's private
library, as encoded within the lockfile.

While lockfiles are normally generated and used with
[`snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md)
/
[`restore()`](https://rstudio.github.io/renv/dev/reference/restore.md),
they can also be edited by hand if so desired. Lockfiles are written as
`.json`, to allow for easy consumption by other tools.

An example lockfile follows:

    {
      "R": {
        "Version": "3.6.1",
        "Repositories": [
          {
            "Name": "CRAN",
            "URL": "https://cloud.r-project.org"
          }
        ]
      },
      "Packages": {
        "markdown": {
          "Package": "markdown",
          "Version": "1.0",
          "Source": "Repository",
          "Repository": "CRAN",
          "Hash": "4584a57f565dd7987d59dda3a02cfb41"
        },
        "mime": {
          "Package": "mime",
          "Version": "0.7",
          "Source": "Repository",
          "Repository": "CRAN",
          "Hash": "908d95ccbfd1dd274073ef07a7c93934"
        }
      }
    }

The sections used within a lockfile are described next.

### renv

Information about the version of renv used to manage this project.

|             |                                                         |
|-------------|---------------------------------------------------------|
| **Version** | The version of the renv package used with this project. |

### R

Properties related to the version of R associated with this project.

|                  |                                          |
|------------------|------------------------------------------|
| **Version**      | The version of R used.                   |
| **Repositories** | The R repositories used in this project. |

### Packages

R package records, capturing the packages used or required by a project
at the time when the lockfile was generated.

|                |                                                                            |
|----------------|----------------------------------------------------------------------------|
| **Package**    | The package name.                                                          |
| **Version**    | The package version.                                                       |
| **Source**     | The location from which this package was retrieved.                        |
| **Repository** | The name of the repository (if any) from which this package was retrieved. |
| **Hash**       | (Optional) A unique hash for this package, used for package caching.       |

Additional remote fields, further describing how the package can be
retrieved from its corresponding source, will also be included as
appropriate (e.g. for packages installed from GitHub).

### Python

Metadata related to the version of Python used with this project (if
any).

|             |                                                                             |
|-------------|-----------------------------------------------------------------------------|
| **Version** | The version of Python being used.                                           |
| **Type**    | The type of Python environment being used ("virtualenv", "conda", "system") |
| **Name**    | The (optional) name of the environment being used.                          |

Note that the `Name` field may be empty. In that case, a project-local
Python environment will be used instead (when not directly using a
system copy of Python).

## Caveats

These functions are primarily intended for expert users – in most cases,
[`snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md)
and
[`restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
are the primariy tools you will need when creating and using lockfiles.

## See also

Other reproducibility:
[`restore()`](https://rstudio.github.io/renv/dev/reference/restore.md),
[`snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md)
