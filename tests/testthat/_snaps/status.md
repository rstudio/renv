# reports if status not possible

    Code
      . <- status()
    Output
      This project does not appear to be using renv.
      Use `renv::init()` to initialize the project.

---

    Code
      . <- status()
    Output
      This project does not contain a lockfile.
      Use `renv::snapshot()` to create a lockfile.

---

    Code
      . <- status()
    Output
      There are no packages installed in the project library.
      Use `renv::restore()` to install the packages defined in lockfile.

# reports when project is synchronised

    Code
      . <- status()
    Output
      No issues found -- the project is in a consistent state.

# reports installation problems with non-installed packages

    Code
      . <- status()
    Output
      The following package(s) are in an inconsistent state:
      
       package installed recorded used
       bread   n         n        y   
       egg     n         y        y   
       oatmeal n         y        ?   
      
      See ?renv::status() for advice on resolving these issues.

# reports synchronisation problems with installed packages

    Code
      . <- status()
    Output
      The following package(s) are in an inconsistent state:
      
       package installed recorded used
       bread   y         n        y   
       egg     y         y        n   
      
      See ?renv::status() for advice on resolving these issues.

# reports version differences

    Code
      . <- status()
    Output
      The following package(s) are out of sync [lockfile != library]:
      
      # CRAN ---
      - egg       [repo: * != CRAN; ver: 2.0.0 != 1.0.0]
      - oatmeal   [repo: * != CRAN; ver: 0.9.0 != 1.0.0]
      
      See ?renv::status() for advice on resolving these issues.

# status() notifies user if R version does not match

    Code
      . <- status()
    Output
      The lockfile was generated with R 1.0.0, but you're using R <r-version>.
      
      See ?renv::status() for advice on resolving these issues.

# status() notifies user if packages are missing and inconsistent

    Code
      . <- status()
    Output
      The following package(s) are used in this project, but are not installed:
      - breakfast
      
      The following package(s) are out of sync [lockfile != library]:
      
      # CRAN ---
      - bread   [1.0.0 != 0.1.0]
      
      See ?renv::status() for advice on resolving these issues.

