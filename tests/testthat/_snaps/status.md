# status reports packages to be installed / changed

    Code
      snapshot()
    Output
      The following required packages are not installed:
      
      	breakfast, toast
      
      Packages must first be installed before renv can snapshot them.
      Consider installing these packages using `renv::install()`.
      Use `renv::dependencies()` to see where this package is used in your project.
      
      The version of R recorded in the lockfile will be updated:
      - R   [* -> 4.2.3]
      
      * Lockfile written to '<wd>/renv.lock'.

---

    Code
      snapshot()
    Output
      The following package(s) will be updated in the lockfile:
      
      # CRAN ===============================
      - bread       [* -> 1.0.0]
      - breakfast   [* -> 1.0.0]
      - oatmeal     [* -> 1.0.0]
      - toast       [* -> 1.0.0]
      
      * Lockfile written to '<wd>/renv.lock'.

---

    Code
      snapshot()
    Output
      The following package(s) will be updated in the lockfile:
      
      # Repository =========================
      - egg   [1.0.0 -> *]
      
      * Lockfile written to '<wd>/renv.lock'.

# status reports packages which are used but not installed

    Code
      status()
    Output
      The following packages are used in this project, but not installed:
      
      	bread
      
      Consider installing these packages -- for example, with `renv::install()`.
      Use `renv::status()` afterwards to re-assess the project state.
      

