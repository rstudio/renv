# Bioconductor packages add BiocManager as a dependency

    Code
      status()
    Output
      The following packages are out of sync:
      
       package      installed recorded used
       BiocGenerics y         n        y   
       BiocManager  y         n        y   
       BiocVersion  y         n        y   
      
      See ?status() for advice

---

    Code
      status()
    Output
      * The project is already synchronized with the lockfile.

# auto-bioc install happens silently

    Code
      install("bioc::BiocGenerics")
    Output
      The following package(s) will be installed:
      
      - BiocGenerics [<version>]
      - BiocVersion  [<version>]
      
      These packages will be installed into "<tempdir>/<renv-library>".
      
      # Installing packages ---
      
      - Installing BiocVersion ...                    OK [copied from cache]
      - Installing BiocGenerics ...                   OK [copied from cache]
      
      Successfully installed 2 packages in XXXX seconds.

