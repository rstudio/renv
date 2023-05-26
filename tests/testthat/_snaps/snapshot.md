# snapshot failures are reported

    Code
      snapshot()
    Output
      The following package(s) are missing their DESCRIPTION files:
      
      	oatmeal [<wd>/renv/library/R-4.2/aarch64-apple-darwin20/oatmeal]
      
      These may be left over from a prior, failed installation attempt.
      Consider removing or reinstalling these packages.
      
      The following required packages are not installed:
      
      	oatmeal
      
      Packages must first be installed before renv can snapshot them.
      Consider installing these packages using `renv::install()`.
      Use `renv::dependencies()` to see where this package is used in your project.
      
      The following package(s) will be updated in the lockfile:
      
      # CRAN ===============================
      - oatmeal   [1.0.0 -> *]
      
      * Lockfile written to '<wd>/renv.lock'.

# broken symlinks are reported

    Code
      snapshot()
    Output
      The following package(s) have broken symlinks into the cache:
      
      	oatmeal
      
      Use `renv::repair()` to try and reinstall these packages.
      
      The following required packages are not installed:
      
      	oatmeal
      
      Packages must first be installed before renv can snapshot them.
      Consider installing these packages using `renv::install()`.
      Use `renv::dependencies()` to see where this package is used in your project.
      
      The following package(s) will be updated in the lockfile:
      
      # CRAN ===============================
      - oatmeal   [1.0.0 -> *]
      
      * Lockfile written to '<wd>/renv.lock'.

# snapshot() warns when required package is not installed

    Code
      snapshot()
    Output
      The following required packages are not installed:
      
      	breakfast
      
      Packages must first be installed before renv can snapshot them.
      Consider installing these packages using `renv::install()`.
      Use `renv::dependencies()` to see where this package is used in your project.
      
      The following package(s) will be updated in the lockfile:
      
      # CRAN ===============================
      - bread       [1.0.0 -> *]
      - breakfast   [1.0.0 -> *]
      - oatmeal     [1.0.0 -> *]
      - toast       [1.0.0 -> *]
      
      * Lockfile written to '<wd>/renv.lock'.

---

    Code
      snapshot()
    Output
      The following required packages are not installed:
      
      	toast  [required by breakfast]
      
      Consider reinstalling these packages before snapshotting the lockfile.
      
    Error <simpleError>
      aborting snapshot due to pre-flight validation failure

# snapshot always reports on R version changes

    Code
      renv_snapshot_report_actions(list(), R4.1, R4.2)
    Output
      The version of R recorded in the lockfile will be updated:
      - R   [4.1 -> 4.2]
      

