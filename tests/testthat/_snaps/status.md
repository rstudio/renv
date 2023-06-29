# reports if status not possible

    Code
      status()
    Output
      Project does not use renv.
      Call renv::init() to setup project to use renv.

---

    Code
      status()
    Output
      Project lacks a lockfile.
      Call renv::snapshot() to create one.

---

    Code
      status()
    Output
      Project lacks a library.
      Call renv::restore() to install packages defined in lockfile.

# reports when project is synchronised

    Code
      status()
    Output
      No issues found.

# reports synchronisation problems with non-installed packages

    Code
      status()
    Output
      The following package(s) are in an inconsistent state:
      
       package installed recorded used
       bread   n         n        y   
       egg     n         y        y   
       oatmeal n         y        ?   
      
      See ?status() for advice on resolving the issues.

# reports synchronisation problems with installed packages

    Code
      status()
    Output
      The following package(s) are in an inconsistent state:
      
       package installed recorded used
       bread   y         n        y   
       egg     y         y        n   
      
      See ?status() for advice on resolving the issues.

# reports version differences

    Code
      status()
    Output
      The following package(s) are out of sync [lockfile -> library]:
      
      # CRAN ---
      - egg       [repo: * -> CRAN; ver: 2.0.0 -> 1.0.0]
      - oatmeal   [repo: * -> CRAN; ver: 0.9.0 -> 1.0.0]
      
      
      See ?status() for advice on resolving the issues.

