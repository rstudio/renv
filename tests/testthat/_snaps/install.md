# renv warns when installing an already-loaded package

    Code
      install("bread@0.1.0")
    Output
      Downloading 1 package(s) and their dependencies
      Done
      
      The following package(s) will be installed:
      
      - bread [0.1.0]
      
      Packages will be installed into "<tempdir>/<renv-library>"
      
      Installing bread [0.1.0] ...
      	OK [copied cache in XXXX seconds]
      Installed 1 package in XXXX seconds.
      
      The following package(s) have been updated:
      
      - bread
      
      Restart your R session to use the newly installed packages.
      

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
      
      Installing oatmeal [1.0.0] ...
      	OK [copied cache in XXXX seconds]
      Installing bread [1.0.0] ...
      	OK [copied cache in XXXX seconds]
      Installing toast [1.0.0] ...
      	OK [copied cache in XXXX seconds]
      Installing breakfast [1.0.0] ...
      	OK [copied cache in XXXX seconds]
      Installed 4 packages in XXXX seconds.

