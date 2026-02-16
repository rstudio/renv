# renv warns when installing an already-loaded package

    Code
      install("bread@0.1.0")
    Output
      The following package(s) will be installed:
      - bread [0.1.0]
      These packages will be installed into "<tempdir>/<renv-library>".
      
      # Installing packages ---
      - Installing bread 0.1.0 ...                    OK [copied from cache in XXs]
      Successfully installed 1 package in XXXX seconds.
      
      The following loaded package(s) have been updated:
      - bread
      Restart your R session to use the new versions.
      

# install has user-friendly output

    Code
      install()
    Output
      The following package(s) will be installed:
      - bread     [1.0.0]
      - breakfast [1.0.0]
      - oatmeal   [1.0.0]
      - toast     [1.0.0]
      These packages will be installed into "<tempdir>/<renv-library>".
      
      # Installing packages ---
      - Downloading bread 1.0.0 ...                   OK
      - Downloading oatmeal 1.0.0 ...                 OK
      - Installing bread 1.0.0 ...                    OK [built from source in XXs]
      - Installing oatmeal 1.0.0 ...                  OK [built from source in XXs]
      - Downloading toast 1.0.0 ...                   OK
      - Installing toast 1.0.0 ...                    OK [built from source in XXs]
      - Downloading breakfast 1.0.0 ...               OK
      - Installing breakfast 1.0.0 ...                OK [built from source in XXs]
      Successfully downloaded 4 packages in XXXX seconds.
      Successfully installed 4 packages in XXXX seconds.

---

    Code
      install()
    Output
      The following package(s) will be installed:
      - bread     [1.0.0]
      - breakfast [1.0.0]
      - oatmeal   [1.0.0]
      - toast     [1.0.0]
      These packages will be installed into "<tempdir>/<renv-library>".
      
      # Installing packages ---
      - Installing bread 1.0.0 ...                    OK [copied from cache in XXs]
      - Installing oatmeal 1.0.0 ...                  OK [copied from cache in XXs]
      - Installing toast 1.0.0 ...                    OK [copied from cache in XXs]
      - Installing breakfast 1.0.0 ...                OK [copied from cache in XXs]
      Successfully installed 4 packages in XXXX seconds.

