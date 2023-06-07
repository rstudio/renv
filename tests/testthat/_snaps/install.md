# renv warns when installing an already-loaded package

    Code
      install("bread@0.1.0")
    Output
      # Downloading package(s) =============
      Nothing needed
      
      # Installing package(s) into "<tempdir>/<renv-library>"
      The following package(s) will be installed:
      
      - bread [0.1.0]
      
      Installing bread ... OK [copied from cache]
      Installed 1 package in XXXX seconds.
      
      The following loaded package(s) have been updated:
      
      - bread
      
      Restart your R session to use the new versions.
      

# install reports on packages about to be installed

    Code
      install()
    Output
      # Downloading package(s) =============
      Nothing needed
      
      # Installing package(s) into "<tempdir>/<renv-library>"
      The following package(s) will be installed:
      
      - bread     [1.0.0]
      - breakfast [1.0.0]
      - oatmeal   [1.0.0]
      - toast     [1.0.0]
      
      Installing oatmeal   ... OK [copied from cache]
      Installing bread     ... OK [copied from cache]
      Installing toast     ... OK [copied from cache]
      Installing breakfast ... OK [copied from cache]
      Installed 4 packages in XXXX seconds.

