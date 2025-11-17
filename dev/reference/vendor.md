# Vendor renv in an R package

Calling `renv:::vendor()` will:

- Compile a vendored copy of renv to `inst/vendor/renv.R`,

- Generate an renv auto-loader at `R/renv.R`.

Using this, projects can take a dependency on renv, and use renv
internals, in a CRAN-compliant way. After vendoring renv, you can use
renv APIs in your package via the embedded renv environment; for
example, you could call the
[`dependencies()`](https://rstudio.github.io/renv/dev/reference/dependencies.md)
function with:

    renv$dependencies()

Be aware that renv internals might change in future releases, so if you
need to rely on renv internal functions, we strongly recommend testing
your usages of these functions to avoid potential breakage.

## Usage

``` r
vendor(version = "main", project = getwd())
```

## Arguments

- version:

  The version of renv to vendor. `renv` sources will be pulled from
  GitHub, and so `version` should refer to either a commit hash or a
  branch name.

- project:

  The project in which renv should be vendored.
