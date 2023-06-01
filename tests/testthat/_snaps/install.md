# renv warns when installing an already-loaded package

    Code
      install("bread@0.1.0")
    Output
      Downloading 1 package(s) and their dependencies
      Done
      
      The following package(s) will be installed:
      
      - bread [0.1.0]
      
      Packages will be installed into "<tempdir>/<renv-library>"
      
      Installing bread ... OK [copied from cache]
      Installed 1 package in XXXX seconds.
      
      The following package(s) have been updated:
      
      - bread [installed version 0.1.0 != loaded version 1.0.0]
      
      Consider restarting the R session and loading the newly-installed packages.
      

# install reports on packages about to be installed

    Code
      install()
    Output
      Downloading 1 package(s) and their dependencies
      Done
      
      The following package(s) will be installed:
      
      - bread     [1.0.0]
      - breakfast [1.0.0]
      - oatmeal   [1.0.0]
      - toast     [1.0.0]
      
      Packages will be installed into "<tempdir>/<renv-library>"
      
      Installing oatmeal   ... OK [copied from cache]
      Installing bread     ... OK [copied from cache]
      Installing toast     ... OK [copied from cache]
      Installing breakfast ... OK [copied from cache]
      Installed 4 packages in XXXX seconds.

