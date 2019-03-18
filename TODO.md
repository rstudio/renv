
- [x] Packrat-style `init()` function, where we discover the R packages used within
      the project, and then initialize a new environment / library with those
      packages. Speed up the process by re-using installed packages in the user
      library if possible.
      
- [x] Warn (error?) when attempting to snapshot a library with missing
      dependencies -- e.g. the library contains 'markdown' but not its
      dependency 'mime'.
      
- [x] Properly handle broken links in the project library (e.g. if the cache
      moved or was mutated for some reason)
  
- [ ] `rehash()` -- tools for validating package hashes + their install location?
      `rehash()` would look at packages in the cache and update their cache
      location if the caching scheme changed.
  
- [ ] `clean()` function to remove ununsed packages from the library. (If the
      cache is enabled, they will remain in the cache).
  
- [ ] Allow users to override the repository used during restore of a
      particular package? (Setting `options(repos)` would suffice here I believe)

- [ ] Use custom `Makevars` file and set some variables that certain packages
      need but don't properly declare? (E.g. older versions of the `maps` package
      need `awk` installed)
      
- [x] `restore()` will attempt to repair the dependency tree during restore;
      e.g. dependent packages in the lockfile will be downloaded and installed
      as required. Should we prompt the user to `snapshot()` afterwards so that
      the newly-reinstalled dependencies can be captured in the lockfile?
      
- [x] Take over the `R_PROFILE`, `R_PROFILE_USER`, `R_ENVIRON` and
      `R_ENVIRON_USER` environment variables? Needed for cases like
      https://github.com/rstudio/packrat/issues/526.

- [x] Make it possible to snapshot arbitrary libraries. [DONE: call `snapshot()`
      on any project to generate a manifest for that project's state]

- [x] `hydrate()` as a general function to discover dependencies and then
      install any missing packages into the active library. (Companion function
      to `init()`)

- [ ] Should we provide for project-specific options? What about download method
      management? [Motivation: users might require `curl` + `http_proxy` settings
      to download packages from CRAN; authentication for GitHub]

- [x] Allow users to ignore certain packages in a project (e.g. those that are
      not on CRAN)
      
- [x] Consider `.renvignore` or similar for controlling what files / directories
      `renv` will crawl when discovering dependencies.

- [x] Properly handle `RENV_DEFAULTS` case (they get lost if the user attempts
      to build and reload a package managed by `renv`, which screws up RStudio's
      notion of the library paths if the project is later deactivated)

- [ ] Audit usages of `renv_file_exists()`. When do we care about broken symlinks?
      `file.exists()` returns FALSE for broken symlinks; `renv_file_exists()`
      returns TRUE.

- [x] De-couple the 'retrieve' + 'install' steps during restore.

- [x] Include recommended packages in the lockfile? (Since R installations on
      Linux may not have these packages available). Or at least confirm that
      packages which depend on recommended packages enter the lockfile.

- [x] Use a single lockfile file, but provide a `history()` function to dig out
      old versions of the lockfile. (Make it clearer to users what needs to be
      committed)

- [x] Handle `NA` mtime in filecached APIs.

- [ ] Implement `bundle()` for packaging up a project, potentially with package
      sources, binaries, or even the library itself for restoration on a new
      machine (potentially lacking internet access).

- [x] Provide tools for upgrading version of `renv` used to manage a project.
      [`renv::upgrade()`]

- [ ] Think about how we might discover 'stale' entries in the cache.

- [x] Handle `renv::init()` case when another project already active (want
      to make sure we use the user library rather than active library)

- [x] `renv::status()` function reporting differences between lockfile + library.

- [ ] Tools for analyizing the cache?

- [x] Python: allow e.g. 'python = TRUE' for initializating a Python project.

- [ ] Python: properly handle conda environments vs. pip.

- [x] Python: teach reticulate how to load virtual environments within renv dir.

- [x] Warn when lockfile contains packages from unknown source?

- [ ] Automatically bootstrap an `renv` installation when loading a project
      if none is available locally?

- [ ] Should `renv::install()` auto-update packages?
