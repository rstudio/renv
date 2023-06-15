# renv warns when installing an already-loaded package

    Code
      install("bread@0.1.0")
    Output
      # Downloading packages -------------------------------------------------------
      [no downloads required]
      
      # Installing packages into "<tempdir>/<renv-library>" ---
      The following package(s) will be installed:
      
      - bread [0.1.0]
      
      Installing bread ... OK [copied from cache]
      Installed 1 package in XXXX seconds.
      
      The following loaded package(s) have been updated:
      
      - bread
      
      Restart your R session to use the new versions.
      

# install has user-friendly output

    Code
      install()
    Output
      # Downloading packages -------------------------------------------------------
      Retrieving '<test-repo>/src/contrib/breakfast_1.0.0.tar.gz' ...
      	OK [downloaded XXXX bytes in XXXX seconds]
      Retrieving '<test-repo>/src/contrib/oatmeal_1.0.0.tar.gz' ...
      	OK [downloaded XXXX bytes in XXXX seconds]
      Retrieving '<test-repo>/src/contrib/toast_1.0.0.tar.gz' ...
      	OK [downloaded XXXX bytes in XXXX seconds]
      Retrieving '<test-repo>/src/contrib/bread_1.0.0.tar.gz' ...
      	OK [downloaded XXXX bytes in XXXX seconds]
      
      # Installing packages into "<tempdir>/<renv-library>" ---
      The following package(s) will be installed:
      
      - bread     [1.0.0]
      - breakfast [1.0.0]
      - oatmeal   [1.0.0]
      - toast     [1.0.0]
      
      Installing oatmeal   ... OK [built from source and cached]
      Installing bread     ... OK [built from source and cached]
      Installing toast     ... OK [built from source and cached]
      Installing breakfast ... OK [built from source and cached]
      Installed 4 packages in XXXX seconds.

---

    Code
      install()
    Output
      # Downloading packages -------------------------------------------------------
      [no downloads required]
      
      # Installing packages into "<tempdir>/<renv-library>" ---
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

