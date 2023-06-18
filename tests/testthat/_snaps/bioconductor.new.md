# Bioconductor packages add BiocManager as a dependency

    Code
      status()
    Output
      The following package(s) are installed, but not recorded in the lockfile:
      
      - BiocGenerics [<version>]
      - BiocManager  [<version>]
      - BiocVersion  [<version>]
      
      Use `renv::snapshot()` to add these packages to the lockfile.
      

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
      
      - Installing BiocVersion ...                    OK [copied cache]
      - Installing BiocGenerics ...                   OK [copied cache]
      
      Successfully installed 2 packages in XXXX seconds.

