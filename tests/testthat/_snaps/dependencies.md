# renv warns when parsing dependencies from a folder with too many files

    Code
      dependencies(errors = "fatal")
    Output
      Finding R package dependencies ... Done!
      WARNING: One or more problems were discovered while enumerating dependencies.
      
      <project-directory>
      -----------------------
      
      ERROR 1: directory "<project-directory>" contains 28 files; consider ignoring this directory
      
      Please see `?renv::dependencies` for more information.
    Error <simpleError>
      one or more errors occurred while enumerating dependencies

