# load() installs packages if needed

    Code
      load()
    Output
      - Project '<wd>' loaded. [renv <version>]
      - None of the packages recorded in the lockfile are installed.
      - Using `renv::restore()` to restore the project library.
      The following package(s) will be updated:
      
      # CRAN ---
      - bread       [* -> 1.0.0]
      - breakfast   [* -> 1.0.0]
      - oatmeal     [* -> 1.0.0]
      - toast       [* -> 1.0.0]
      
      # Installing packages ---
      
      - Installing bread ...                          OK [linked from cache in XXs]
      - Installing oatmeal ...                        OK [linked from cache in XXs]
      - Installing toast ...                          OK [linked from cache in XXs]
      - Installing breakfast ...                      OK [linked from cache in XXs]
      

# load() reports on problems

    Code
      load()
    Output
      - Project '<wd>' loaded. [renv <version>]
      - The project is out-of-sync -- use `renv::status()` for details.

