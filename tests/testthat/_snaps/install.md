# renv warns when installing an already-loaded package

    Code
      install("bread@0.1.0")
    Output
      The following package(s) will be installed:
      
      - bread [0.1.0]
      
      These packages will be installed into "<tempdir>/<renv-library>".
      
      # Installing packages ---
      
      - Installing bread ...                          OK [copied from cache in XXs]
      
      Successfully installed 1 package in XXXX seconds.
      
      The following loaded package(s) have been updated:
      
      - bread
      
      Restart your R session to use the new versions.
      

# install has user-friendly output

    Code
      install()
    Output
      # Downloading packages ---
      
      - Downloading breakfast from CRAN ...           OK [XXXX bytes in XXs]
      - Downloading oatmeal from CRAN ...             OK [XXXX bytes in XXs]
      - Downloading toast from CRAN ...               OK [XXXX bytes in XXs]
      - Downloading bread from CRAN ...               OK [XXXX bytes in XXs]
      
      The following package(s) will be installed:
      
      - bread     [1.0.0]
      - breakfast [1.0.0]
      - oatmeal   [1.0.0]
      - toast     [1.0.0]
      
      These packages will be installed into "<tempdir>/<renv-library>".
      
      # Installing packages ---
      
      - Installing oatmeal ...                        OK [built from source and cached in XXs]
      - Installing bread ...                          OK [built from source and cached in XXs]
      - Installing toast ...                          OK [built from source and cached in XXs]
      - Installing breakfast ...                      OK [built from source and cached in XXs]
      
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
      
      - Installing oatmeal ...                        OK [copied from cache in XXs]
      - Installing bread ...                          OK [copied from cache in XXs]
      - Installing toast ...                          OK [copied from cache in XXs]
      - Installing breakfast ...                      OK [copied from cache in XXs]
      
      Successfully installed 4 packages in XXXX seconds.

