
# renv 0.8.3 (UNRELEASED)

* Fixes for checks run on CRAN.

* renv will now search for Rtools in more locations. (#225)

* `renv::load()` now ensures that the version of `renv` associated with
  the loaded project is loaded when possible. In addition, experimental
  support for switching between projects with `renv::load()` has been
  implemented. (#229)

* `renv::dependencies()` no longer treats folders named with the extension
  `.Rmd` as though they were regular files. (#228)

* It is now possible to install source packages contained within `.zip`
  archives using `renv::install()`.

* Fixed an issue where attempts to call `renv::restore()` with the path to the
  lockfile explicitly provided would fail. (#227)

# renv 0.8.2

* Further fixes for checks run on CRAN.

# renv 0.8.1

* Fixes for checks run on CRAN.

# renv 0.8.0

* Initial CRAN release.
