# Capture and re-use dependencies within a `.R`, `.Rmd` or `.qmd`

Together, `embed()` and `use()` provide a lightweight way to specify and
restore package versions within a file. `use()` is a lightweight
lockfile specification that `embed()` can automatically generate and
insert into a script or document.

Calling `embed()` inspects the dependencies of the specified document
then generates and inserts a call to `use()` that looks something like
this:

    renv::use(
      "digest@0.6.30",
      "rlang@0.3.4"
    )

When you next run your R script or render your `.Rmd` or `.qmd`, `use()`
will:

1.  Create a temporary library path,

2.  Install the requested packages and their recursive dependencies into
    that library,

3.  Activate the library, so it's used for the rest of the script.

### Manual usage

You can also create calls to `use()` yourself, either specifying the
packages needed by hand, or by supplying the path to a lockfile,
`renv::use(lockfile = "/path/to/renv.lock")`.

This can be useful in projects where you'd like to associate different
lockfiles with different documents, as in a blog where you want each
post to capture the dependencies at the time of writing. Once you've
finished writing each, the post, you can use
`renv::snapshot(lockfile = "/path/to/renv.lock")` to "save" the state
that was active while authoring that bost, and then use
`renv::use(lockfile = "/path/to/renv.lock")` in that document to ensure
the blog post always uses those dependencies onfuture renders.

`renv::use()` is inspired in part by the
[groundhog](https://groundhogr.com/) package, which also allows one to
specify a script's R package requirements within that same R script.

## Usage

``` r
embed(path = NULL, ..., lockfile = NULL, project = NULL)

use(
  ...,
  lockfile = NULL,
  library = NULL,
  isolate = TRUE,
  sandbox = TRUE,
  attach = FALSE,
  verbose = TRUE
)
```

## Arguments

- path:

  The path to an R or R Markdown script. The default will use the
  current document, if running within RStudio.

- ...:

  The R packages to be used with this script. Ignored if `lockfile` is
  non-`NULL`.

- lockfile:

  The lockfile to use. When supplied, renv will use the packages as
  declared in the lockfile.

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

- library:

  The library path into which the requested packages should be
  installed. When `NULL` (the default), a library path within the R
  temporary directory will be generated and used. Note that this same
  library path will be re-used on future calls to `renv::use()`,
  allowing `renv::use()` to be used multiple times within a single
  script.

- isolate:

  Boolean; should the active library paths be included in the set of
  library paths activated for this script? Set this to `TRUE` if you
  only want the packages provided to `renv::use()` to be visible on the
  library paths.

- sandbox:

  Should the system library be sandboxed? See the sandbox documentation
  in [config](https://rstudio.github.io/renv/dev/reference/config.md)
  for more details. You can also provide an explicit sandbox path if you
  want to configure where `renv::use()` generates its sandbox. By
  default, the sandbox is generated within the R temporary directory.

- attach:

  Boolean; should the set of requested packages be automatically
  attached? If `TRUE`, packages will be loaded and attached via a call
  to [`library()`](https://rdrr.io/r/base/library.html) after install.
  Ignored if `lockfile` is non-`NULL`.

- verbose:

  Boolean; be verbose while installing packages?

## Value

This function is normally called for its side effects.
