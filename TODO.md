
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
      this clear.
  
- [x] Make it possible to 'fork' a global virtual environment and make it
      project-local instead. (So that the user can mutate the library without
      affecting other projects using that global library)
  
- [ ] Properly handle broken links in the project library (e.g. if the cache
      moved or was mutated for some reason)
  
- [ ] `rehash()` -- tools for validating package hashes + their install location?
      `rehash()` would look at packages in the cache and update their cache
      location if the caching scheme changed.
  
- [ ] `clean()` function to remove ununsed packages from the library. (If the
      cache is enabled, they will remain in the cache)
  
- [ ] Allow users to override the repository used during restore of a
      particular package?

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
