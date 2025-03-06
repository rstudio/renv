# renv warns when large number of files found in total

    Code
      . <- dependencies()
    Output
      A large number of files (7 in total) have been discovered.
      It may take renv a long time to scan these files for dependencies.
      Consider using .renvignore to ignore irrelevant files.
      See `?renv::dependencies` for more information.
      Set `options(renv.config.dependencies.limit = Inf)` to disable this warning.
      
      Finding R package dependencies ... Done!

# renv warns when large number of files found in one directory

    Code
      . <- dependencies()
    Output
      A large number of files (11 in total) have been discovered.
      It may take renv a long time to scan these files for dependencies.
      Consider using .renvignore to ignore irrelevant files.
      See `?renv::dependencies` for more information.
      Set `options(renv.config.dependencies.limit = Inf)` to disable this warning.
      
      Finding R package dependencies ... Done!
      WARNING: One or more problems were discovered while enumerating dependencies.
      
      # <wd-name> ---
      Error: directory contains 12 files; consider ignoring this directory
      
      Please see `?renv::dependencies` for more information.
      

# dependencies() notifies the user if directories contain lots of files

    Code
      . <- renv_snapshot_dependencies(project)
    Output
      
      NOTE: Dependency discovery took XXXX seconds during snapshot.
      Consider using .renvignore to ignore files, or switching to explicit snapshots.
      See `?renv::dependencies` for more information.
      
      - <wd>/data: 200 files
      

