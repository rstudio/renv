# renv warns when installing an already-loaded package

    Code
      install("bread@0.1.0")
    Output
      The following package(s) will be installed:
      - bread [0.1.0]
      These packages will be installed into "<tempdir>/<renv-library>".
      
      # Installing packages ---
      [32m✔[0m bread 0.1.0                              [copied from cache]
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
      
      # Downloading packages ---
      [32m✔[0m bread 1.0.0                             
      [32m✔[0m breakfast 1.0.0                         
      [32m✔[0m oatmeal 1.0.0                           
      [32m✔[0m toast 1.0.0                             
      Successfully downloaded 4 packages in XXXX seconds.
      
      # Installing packages ---
      [32m✔[0m bread 1.0.0                              [built from source]
      [32m✔[0m breakfast 1.0.0                          [built from source]
      [32m✔[0m oatmeal 1.0.0                            [built from source]
      [32m✔[0m toast 1.0.0                              [built from source]
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
      [32m✔[0m bread 1.0.0                              [copied from cache]
      [32m✔[0m breakfast 1.0.0                          [copied from cache]
      [32m✔[0m oatmeal 1.0.0                            [copied from cache]
      [32m✔[0m toast 1.0.0                              [copied from cache]
      Successfully installed 4 packages in XXXX seconds.

