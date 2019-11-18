
# renv 0.8.4 (UNRELEASED)

* Fixed an issue where `renv` would, in some cases, copy rather than link from
  the package cache when the library path had been customized with the
  `RENV_PATHS_LIBRARY` environment variable. (#245)

* The method `renv` uses when copying directories can now be customized. When
  copying directories, `renv` now by default uses `robocopy` on Windows, and
  `cp` on Unix. This should improve robustness when attempting to copy files
  in some contexts; e.g. when copying across network shares.

* `renv` now tracks the version of Bioconductor used within a project
  (if applicable), and uses that when retrieving the set of repositories
  to be used during `renv::restore()`.

* `renv::dependencies()` can now parse R package dependencies declared and
  used by the `modules` package. (#238, @labriola)

* Fixed an issue where `renv::restore()` could fail in Docker environments,
  usually with an error message like 'Invalid cross-device link'. (#243)

* `renv::install()` disables staged package install when running with the
  Windows Subsystem for Linux. (#239)

# renv 0.8.3

* `renv::dependencies()` gains a new argument `dev`, indicating whether
  development dependencies should also be included in the set of discovered
  package dependencies. By default, only runtime dependencies will be reported.

* `renv` has gained the function `renv::diagnostics()`, which can occasionally
  be useful in understanding and diagnosing `renv` (mis)behaviors.

* `renv::equip()` can now be used on macOS to install the R LLVM toolchain
  normally used when compiling packages from source. `renv` will also use
  this toolchain as appropriate when building packages from source.

* `renv::install()` now provides a custom Makevars when building packages on
  macOS with Apple Clang, to avoid issues due to the use of '-fopenmp' during
  compilation.

* `renv::install()` now respects explicit version requests when discovered
  in a project's DESCRIPTION file. (#233)

* Fixed an issue where `renv:::actions()` would fail to report any actions if
  the project lockfile was empty. (#232)

* When using `renv` for R package development, `renv` will no longer attempt to
  write the package being developed to the lockfile. (#231)

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
