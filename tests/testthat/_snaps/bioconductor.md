# Bioconductor packages add BiocManager as a dependency

    Code
      status()
    Output
      The following package(s) are installed, but not recorded in the lockfile:
      
      - BiocGenerics [<version>]
      - BiocManager  [<version>]
      
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
      # Downloading packages -------------------------------------------------------
      [no downloads required]
      
      # Installing packages into "<tempdir>/<renv-library>" ---
      The following package(s) will be installed:
      
      - BiocGenerics [<version>]
      
      Installing BiocGenerics ... OK [copied from cache]
      Installed 1 package in XXXX seconds.

