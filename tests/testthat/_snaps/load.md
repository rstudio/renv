# load() installs packages if needed

    Code
      load()
    Output
      # Loading renv [<version>] ---
      - Project '<wd>' loaded.
      - None of the packages recorded in the lockfile are installed.
      - Using `renv::restore()` to restore the project library.
      The following package(s) will be updated:
      
      # CRAN ---
      - bread       [* -> 1.0.0]
      - breakfast   [* -> 1.0.0]
      - oatmeal     [* -> 1.0.0]
      - toast       [* -> 1.0.0]
      
      # Installing packages ---
      
      - Installing bread ...                          OK [linked from cache]
      - Installing oatmeal ...                        OK [linked from cache]
      - Installing toast ...                          OK [linked from cache]
      - Installing breakfast ...                      OK [linked from cache]
      

# load() reports on problems

    Code
      load()
    Output
      # Loading renv [<version>] ---
      - Project '<wd>' loaded.
      - The project is currently out-of-sync.
      - Use `renv::status()` for more details.

