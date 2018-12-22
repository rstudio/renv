
- [x] Packrat-style `init()` function, where we discover the R packages used within
      the project, and then initialize a new environment / library with those
      packages. Speed up the process by re-using installed packages in the user
      library if possible.
  
- [ ] Make it possible to 'fork' a global virtual environment and make it
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
