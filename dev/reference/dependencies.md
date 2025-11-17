# Find R package dependencies in a project

`dependencies()` will scan files within your project, looking for R
files and the packages used within those R files. This is done primarily
by parsing the code and looking for calls of the form
[`library(package)`](https://rdrr.io/r/base/library.html),
[`require(package)`](https://rdrr.io/r/base/library.html),
[`requireNamespace("package")`](https://rdrr.io/r/base/ns-load.html),
and `package::method()`. renv also supports package loading with
[box](https://cran.r-project.org/package=box) (`box::use(...)`) and
[pacman](https://cran.r-project.org/package=pacman)
(`pacman::p_load(...)`).

For R package projects, `renv` will also detect dependencies expressed
in the `DESCRIPTION` file. For projects using Python, R dependencies
within the R code chunks of your project's `.ipynb` files will also be
used.

Note that the
[`rmarkdown`](https://pkgs.rstudio.com/rmarkdown/reference/rmarkdown-package.html)
package is required in order to scan dependencies in R Markdown files.

## Usage

``` r
dependencies(
  path = getwd(),
  root = NULL,
  ...,
  quiet = NULL,
  progress = TRUE,
  errors = c("reported", "fatal", "ignored"),
  dev = FALSE
)
```

## Arguments

- path:

  The path to a `.R`, `.Rmd`, `.qmd`, `DESCRIPTION`, a directory
  containing such files, or an R function. The default uses all files
  found within the current working directory and its children.

- root:

  The root directory to be used for dependency discovery. Defaults to
  the active project directory. You may need to set this explicitly to
  ensure that your project's `.renvignore`s (if any) are properly
  handled.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- quiet:

  Boolean; be quiet while checking for dependencies? Setting
  `quiet = TRUE` is equivalent to setting `progress = FALSE` and
  `errors = "ignored"`, and overrides those options when not `NULL`.

- progress:

  Boolean; report progress output while enumerating dependencies?

- errors:

  How should errors that occur during dependency enumeration be handled?

  - `"reported"` (the default): errors are reported to the user, but
    otherwise ignored.

  - `"fatal"`: errors are fatal and stop execution.

  - `"ignored"`: errors are ignored and not reported to the user.

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

An R `data.frame` of discovered dependencies, mapping inferred package
names to the files in which they were discovered. Note that the
`Package` field might name a package remote, rather than just a plain
package name.

## Missing dependencies

`dependencies()` uses static analysis to determine which packages are
used by your project. This means that it inspects, but doesn't run, the
R code in your project. Static analysis generally works well, but is not
100% reliable in detecting the packages required by your project. For
example, `renv` is unable to detect this kind of usage:

    for (package in c("dplyr", "ggplot2")) {
      library(package, character.only = TRUE)
    }

It also can't generally tell if one of the packages you use, uses one of
its suggested packages. For example, the `tidyr::separate_wider_delim()`
function requires the `stringr` package, but `stringr` is only
suggested, not required, by `tidyr`.

If you find that renv's dependency discovery misses one or more packages
that you actually use in your project, one escape hatch is to include a
file called `_dependencies.R` that includes straightforward library
calls:

    library(dplyr)
    library(ggplot2)
    library(stringr)

## Ignoring files

By default, renv will read your project's `.gitignore`s (if present) to
determine whether certain files or folders should be included when
traversing directories. If preferred, you can also create a
`.renvignore` file (with entries of the same format as a standard
`.gitignore` file) to tell renv which files to ignore within a
directory. If both `.renvignore` and `.gitignore` exist within a folder,
the `.renvignore` will be used in lieu of the `.gitignore`.

See <https://git-scm.com/docs/gitignore> for documentation on the
`.gitignore` format. Some simple examples here:

    # ignore all R Markdown files
    *.Rmd

    # ignore all data folders
    data/

    # ignore only data folders from the root of the project
    /data/

Using ignore files is important if your project contains a large number
of files; for example, if you have a `data/` directory containing many
text files.

### Profile-specific Ignore Rules

Profile-specific sections are also supported in `.renvignore` files.
These sections are marked with a comment header of the form `#| <code>`,
where `<code>` is R code that indicates if this section of the
`.renvignore` should apply. The `profile` variable is set to the same
value as the current profile, or `"default"` if the default profile (no
profile) is selected. See
[`vignette("profiles", package = "renv")`](https://rstudio.github.io/renv/dev/articles/profiles.md)
for more information on profiles.

    # ignore all directories by default
    */

    #| profile == "default"
    !default

    #| profile == "extra"
    !extra

Note that the first section in a `.renvignore` file implicitly applies
to all profiles.

## Errors

renv's attempts to enumerate package dependencies in your project can
fail – most commonly, because of failures when attempting to parse your
R code. You can use the `errors` argument to suppress these problems,
but a more robust solution is tell renv not to look at the problematic
code. As well as using `.renvignore`, as described above, you can also
suppress errors discovered within individual `.Rmd` chunks by including
`renv.ignore=TRUE` in the chunk header. For example:

    ```{r chunk-label, renv.ignore=TRUE}
    # code in this chunk will be ignored by renv
    ```

Similarly, if you'd like renv to parse a chunk that is otherwise ignored
(e.g. because it has `eval=FALSE` as a chunk header), you can set:

    ```{r chunk-label, eval=FALSE, renv.ignore=FALSE}
    # code in this chunk will _not_ be ignored
    ```

## Development dependencies

renv has some support for distinguishing between development and
run-time dependencies. For example, your Shiny app might rely on
[ggplot2](https://ggplot2.tidyverse.org) (a run-time dependency) but
while you use [usethis](https://usethis.r-lib.org) during development,
your app doesn't need it to run (i.e. it's only a development
dependency).

You can record development dependencies by listing them in the
`Suggests` field of your project's `DESCRIPTION` file. Development
dependencies will be installed by
[`install()`](https://rstudio.github.io/renv/dev/reference/install.md)
(when called without arguments) but will not be tracked in the project
snapshot. If you need greater control, you can also try project profiles
as discussed in
[`vignette("profiles")`](https://rstudio.github.io/renv/dev/articles/profiles.md).

## Examples

``` r
if (FALSE) { # \dontrun{

# find R package dependencies in the current directory
renv::dependencies()

} # }
```
