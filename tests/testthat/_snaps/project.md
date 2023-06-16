# renv_project_synchronized_check() installs packages if needed

    Code
      ok <- renv_project_synchronized_check()
    Output
      * None of the packages recorded in the lockfile are installed.
      * Using `renv::restore()` to restore the project library.
      The following package(s) will be updated:
      
      # CRAN ---
      - bread       [* -> 1.0.0]
      - breakfast   [* -> 1.0.0]
      - oatmeal     [* -> 1.0.0]
      - toast       [* -> 1.0.0]
      
      # Downloading packages ---
      [no downloads required]
      
      # Installing packages ---

      - Installing bread ...                          OK [linked from cache]
      - Installing oatmeal ...                        OK [linked from cache]
      - Installing toast ...                          OK [linked from cache]
      - Installing breakfast ...                      OK [linked from cache]

