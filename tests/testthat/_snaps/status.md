# reports synchronisation problems with non-installed packages

    Code
      status()
    Output
      The following package(s) are out of sync:
      
       package installed recorded used
       bread   n         n        y   
       egg     n         y        y   
       oatmeal n         y        ?   
      
      See ?status() for advice on resolving the problems.

# reports synchronisation problems with installed packages

    Code
      status()
    Output
      The following package(s) are out of sync:
      
       package installed recorded used
       bread   y         n        y   
       egg     y         y        n   
      
      See ?status() for advice on resolving the problems.

# reports version differences

    Code
      status()
    Output
      The following package(s) are out of sync [lockfile -> library]:
      
      # CRAN ---
      - egg       [repo: * -> CRAN; ver: 2.0.0 -> 1.0.0]
      - oatmeal   [repo: * -> CRAN; ver: 0.9.0 -> 1.0.0]
      
      
      See ?status() for advice on resolving the problems.

