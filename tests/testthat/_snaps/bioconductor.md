# Bioconductor packages add BiocManager as a dependency

    Code
      status()
    Output
      The following package(s) are in an inconsistent state:
      
       package      installed recorded used
       BiocGenerics y         n        y   
       BiocManager  y         n        y   
       BiocVersion  y         n        y   
      
      See ?renv::status() for advice on resolving these issues.

---

    Code
      status()
    Output
      No issues found -- the project is in a consistent state.

# auto-bioc install happens silently

    Code
      install("bioc::BiocGenerics")
    Output
      The following package(s) will be installed:
      - BiocGenerics [<version>]
      - BiocVersion  [<version>]
      These packages will be installed into "<tempdir>/<renv-library>".
      
      # Installing packages ---
      - Installing BiocVersion ...                    OK [copied from cache in XXs]
      - Installing BiocGenerics ...                   OK [copied from cache in XXs]
      Successfully installed 2 packages in XXXX seconds.

