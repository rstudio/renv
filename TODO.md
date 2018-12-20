
- Packrat-style `init()` function, where we discover the R packages used within
  the project, and then initialize a new environment / library with those
  packages. Speed up the process by re-using installed packages in the user
  library if possible.
  
- `clean()` function to remove ununsed packages from the library. (If the
  cache is enabled, they will remain in the cache)

- Document common use cases and how to accomplish:
  - Use development library on top of user library.
  - Isolate project (Packrat style).
  - Fork an existing environment to use locally.
