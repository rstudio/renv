# not installed/recorded/used

    Code
      status()
    Output
      The following packages are recorded in the lockfile, but not installed:
              _
        bread   [1.0.0]
      
      Use `renv::restore()` to restore the packages recorded in the lockfile.
      

# installed/not recorded/used

    Code
      status()
    Output
      The following package(s) are installed, but not recorded in the lockfile:
              _
        bread   [1.0.0]
      
      Use `renv::snapshot()` to add these packages to the lockfile.
      

# not installed/*/used

    Code
      status()
    Output
      The following packages are used in this project, but not installed:
      
      	bread
      
      Consider installing these packages -- for example, with `renv::install()`.
      Use `renv::status()` afterwards to re-assess the project state.
      

---

    Code
      status()
    Output
      The following packages are recorded in the lockfile, but not installed:
              _
        bread   [1.0.0]
      
      Use `renv::restore()` to restore the packages recorded in the lockfile.
      

# */recorded/not used

    Code
      status()
    Output
      The following packages are recorded in the lockfile, but do not appear to be used in this project:
            _
        egg   [1.0.0]
      
      Use `renv::snapshot()` if you'd like to remove these packages from the lockfile.
      

# installed/*/not used

    Code
      status()
    Output
      The following packages are installed but not used.
      - egg [1.0.0]
      
      * The project is already synchronized with the lockfile.

# other changes

    Code
      status()
    Output
      The following package(s) are out of sync:
      
         Package   Lockfile Version   Library Version
             egg              2.0.0             1.0.0
         oatmeal              0.9.0             1.0.0
      
      Use `renv::snapshot()` to save the state of your library to the lockfile.
      Use `renv::restore()` to restore your library from the lockfile.
      

