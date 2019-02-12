
- [x] Packrat-style `init()` function, where we discover the R packages used within
      the project, and then initialize a new environment / library with those
      packages. Speed up the process by re-using installed packages in the user
      library if possible.
      
- [x] Warn (error?) when attempting to snapshot a library with missing
      dependencies -- e.g. the library contains 'markdown' but not its
      dependency 'mime'.
      
- [ ] Think about what it really means to 'restore' a virtual environment. Is
      it clear to the user what this action will do? The Packrat model here is
      actually simpler since 'restore()' always means 'restore my local project
      library', whereas in renv it means 'restore some virtual environment with
      some name to some state'. It will take some extra communication to make
      this clear. In particular, it is perhaps not obvious that one virtual
      environment can have multiple libraries, and different packages can
      and will be installed into the library they were originally in at
      snapshot time.
  
- [x] Make it possible to 'fork' a global virtual environment and make it
      project-local instead. (So that the user can mutate the library without
      affecting other projects using that global library)
  
- [ ] Properly handle broken links in the project library (e.g. if the cache
      moved or was mutated for some reason)
  
- [ ] `rehash()` -- tools for validating package hashes + their install location?
      `rehash()` would look at packages in the cache and update their cache
      location if the caching scheme changed.
  
- [x] `clean()` function to remove ununsed packages from the library. (If the
      cache is enabled, they will remain in the cache). [WONTFIX: because
      R libraries are no longer private to a particular project, except in the
      case of local environments, one cannot safely remove packages as this
      action could affect other projects bound to that environment]
  
- [ ] Allow users to override the repository used during restore of a
      particular package? (Setting `options(repos)` would suffice here I believe)

- [ ] Document common use cases and how to accomplish:
      - Use development library on top of user library.
      - Isolate project (Packrat style).
      - Fork an existing environment to use locally.

- [ ] Use custom `Makevars` file and set some variables that certain packages
      need but don't properly declare? (E.g. older versions of the `maps` package
      need `awk` installed)
      
- [ ] `restore()` will attempt to repair the dependency tree during restore;
      e.g. dependent packages in the manifest will be downloaded and installed
      as required. Should we prompt the user to `snapshot()` afterwards so that
      the newly-reinstalled dependencies can be captured in the manifest?
      
- [x] Take over the `R_PROFILE`, `R_PROFILE_USER`, `R_ENVIRON` and
      `R_ENVIRON_USER` environment variables? Needed for cases like
      https://github.com/rstudio/packrat/issues/526.

- [ ] Make it possible to snapshot arbitrary libraries. The main challenge
      is handling the library name / path, since we almost always assume the
      library path must resolve to an 'renv' library path. Inspect usages
      of '$Library'.

- [ ] `hydrate()` as a general function to discover dependencies and then
      install any missing packages into the active library. (Companion function
      to `init()`)

- [ ] Should we provide for project-specific options? What about download method
      management?

- [ ] Allow users to ignore certain packages in a project (e.g. those that are
      not on CRAN)
      
- [ ] Consider `.renvignore` or similar for controlling what files / directories
      `renv` will crawl when discovering dependencies.

- [x] Properly handle `RENV_DEFAULTS` case (they get lost if the user attempts
      to build and reload a package managed by `renv`, which screws up RStudio's
      notion of the library paths if the project is later deactivated)

- [ ] What if I want to overlay a private (local) library on top of a global library?

- [ ] Audit usages of `renv_file_exists()`. When do we care about broken symlinks?
      `file.exists()` returns FALSE for broken symlinks; `renv_file_exists()`
      returns TRUE.

- [ ] De-couple the 'retrieve' + 'install' steps during restore.

- [ ] Include recommended packages in the manifest? (Since R installations on
      Linux may not have these packages available). Or at least confirm that
      packages which depend on recommended packages enter the manifest.

- [ ] Use a single manifest file, but provide a `history()` function to dig out
      old versions of the manifest. (Make it clearer to users what needs to be
      committed)
