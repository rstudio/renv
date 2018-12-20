# renv (UNDER DEVELOPMENT)


## Overview

Create and bind projects to R virtual environments. With `renv`, you can bind
particular R projects to different virtual environments, each containing its own
set of R libraries.

## Workflows

`renv` allows for a number of different workflows:

1. Similar to [Packrat](https://rstudio.github.io/packrat/), a project can be
   initialized from scratch by inferring the packages used within a project,
   and then preparing a project-local R library. The `renv` approach is much
   faster, as we avoid downloading and building packages from source whenever
   possible.
   
``` r
renv::init()
```
   
2. Projects can share a global set of R libraries. For example, projects under
   active development could use a 'develop' library, while projects used in
   production could use a 'production' library. This is similar to the model
   already available in R with site libraries and user libraries, but this
   package normalizes and standardizes the model across different platforms.
   
``` r
renv::create("develop")
renv::activate("develop")
```

3. Projects can opt-in to using the user library, with their own overlay of
   development packages. This can be useful when working on the development
   version of a package, which itself has dev-version package dependencies,
   but you want to avoid clobbering other packages available in the user
   library.

``` r
renv::create(r_libs_overlay = TRUE, local = TRUE)
renv::activate(local = TRUE)
```

## Usage

