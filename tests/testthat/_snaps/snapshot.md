# snapshot failures are reported

    Code
      snapshot()
    Output
      The following package(s) are missing their DESCRIPTION files:
      - oatmeal [<wd>/renv/library/<platform-prefix>/oatmeal]
      These may be left over from a prior, failed installation attempt.
      Consider removing or reinstalling these packages.
      
      The following required packages are not installed:
      - oatmeal
      Packages must first be installed before renv can snapshot them.
      Use `renv::dependencies()` to see where this package is used in your project.
      
      What do you want to do?
      
      1: Snapshot, just using the currently installed packages.
      2: Install the packages, then snapshot.
      3: Cancel, and resolve the situation on your own.
      
      Selection: 1
      
      The following package(s) will be updated in the lockfile:
      
      # CRAN ---
      - oatmeal   [1.0.0 -> *]
      
      - Lockfile written to "<wd>/renv.lock".

# broken symlinks are reported

    Code
      snapshot()
    Output
      The following package(s) have broken symlinks into the cache:
      - oatmeal
      Use `renv::repair()` to try and reinstall these packages.
      
      The following required packages are not installed:
      - oatmeal
      Packages must first be installed before renv can snapshot them.
      Use `renv::dependencies()` to see where this package is used in your project.
      
      What do you want to do?
      
      1: Snapshot, just using the currently installed packages.
      2: Install the packages, then snapshot.
      3: Cancel, and resolve the situation on your own.
      
      Selection: 1
      
      The following package(s) will be updated in the lockfile:
      
      # CRAN ---
      - oatmeal   [1.0.0 -> *]
      
      - Lockfile written to "<wd>/renv.lock".

# snapshot warns about unsatisfied dependencies

    Code
      snapshot()
    Output
      The following package(s) have unsatisfied dependencies:
      - toast requires bread (> 1.0.0), but version 1.0.0 is installed
      Consider updating the required dependencies as appropriate.
      
    Error <simpleError>
      aborting snapshot due to pre-flight validation failure

# renv reports missing packages in explicit snapshots

    Code
      snapshot(type = "explicit")
    Output
      The following required packages are not installed:
      - breakfast
      Packages must first be installed before renv can snapshot them.
      If these packages are no longer required, consider removing them from your DESCRIPTION file.
      
      What do you want to do?
      
      1: Snapshot, just using the currently installed packages.
      2: Install the packages, then snapshot.
      3: Cancel, and resolve the situation on your own.
      
      Selection: 1
      
      - The lockfile is already up to date.

# snapshot() warns when required package is not installed

    Code
      snapshot()
    Output
      The following required packages are not installed:
      - breakfast
      Packages must first be installed before renv can snapshot them.
      Use `renv::dependencies()` to see where this package is used in your project.
      
      What do you want to do?
      
      1: Snapshot, just using the currently installed packages.
      2: Install the packages, then snapshot.
      3: Cancel, and resolve the situation on your own.
      
      Selection: 1
      
      The following package(s) will be updated in the lockfile:
      
      # CRAN ---
      - bread       [1.0.0 -> *]
      - breakfast   [1.0.0 -> *]
      - oatmeal     [1.0.0 -> *]
      - toast       [1.0.0 -> *]
      
      - Lockfile written to "<wd>/renv.lock".

---

    Code
      snapshot()
    Output
      The following required packages are not installed:
      - toast  [required by breakfast]
      Consider reinstalling these packages before snapshotting the lockfile.
      
    Error <simpleError>
      aborting snapshot due to pre-flight validation failure

# snapshot always reports on R version changes

    Code
      renv_snapshot_report_actions(list(), R4.1, R4.2)
    Output
      The version of R recorded in the lockfile will be updated:
      - R   [4.1 -> 4.2]
      

# user can choose to install missing packages

    Code
      snapshot()
    Output
      The following required packages are not installed:
      - egg
      Packages must first be installed before renv can snapshot them.
      Use `renv::dependencies()` to see where this package is used in your project.
      
      What do you want to do?
      
      1: Snapshot, just using the currently installed packages.
      2: Install the packages, then snapshot.
      3: Cancel, and resolve the situation on your own.
      
      Selection: 2
      
      # Downloading packages ---
      - Downloading egg from CRAN ...                 OK [XXXX bytes in XXs]
      Successfully downloaded 1 package in XXXX seconds.
      
      The following package(s) will be installed:
      - egg [1.0.0]
      These packages will be installed into "<tempdir>/<renv-library>".
      
      # Installing packages ---
      - Installing egg ...                            OK [built from source and cached in XXs]
      Successfully installed 1 package in XXXX seconds.
      The following package(s) will be updated in the lockfile:
      
      # CRAN ---
      - egg   [* -> 1.0.0]
      
      The version of R recorded in the lockfile will be updated:
      - R     [* -> <r-version>]
      
      - Lockfile written to "<wd>/renv.lock".

# automatic snapshot works as expected

    Code
      renv_snapshot_task()
    Output
      - Automatic snapshot has updated '<wd>/renv.lock'.

# we report if dependency discover during snapshot() is slow

    Code
      . <- snapshot()
    Output
      
      NOTE: Dependency discovery took XXXX seconds during snapshot.
      Consider using .renvignore to ignore files, or switching to explicit snapshots.
      See `?renv::dependencies` for more information.
      
      - The lockfile is already up to date.

# failures in automatic snapshots disable automatic snapshots

    Code
      renv_snapshot_task()
    Output
      Error generating automatic snapshot: simulated failure in snapshot task
      Automatic snapshots will be disabled. Use `renv::snapshot()` to manually update the lockfile.

# snapshot() reports missing packages even if renv.verbose is FALSE

    Code
      . <- snapshot(force = TRUE)
    Output
      The following required packages are not installed:
      - bread
      Packages must first be installed before renv can snapshot them.
      Use `renv::dependencies()` to see where this package is used in your project.
      
      What do you want to do?
      
      1: Snapshot, just using the currently installed packages.
      2: Install the packages, then snapshot.
      3: Cancel, and resolve the situation on your own.
      
      Selection: 1
      
      - The lockfile is already up to date.

