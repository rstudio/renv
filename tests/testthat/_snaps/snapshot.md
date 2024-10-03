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

