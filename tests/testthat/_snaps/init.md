# attempts to initialize a project with a missing package is okay

    Code
      init()
    Output
      - Resolving missing dependencies ... 
      # Downloading packages ---
      The following package(s) were not installed successfully:
      - [missing]: failed to download
      You may need to manually download and install these packages.
      
      The version of R recorded in the lockfile will be updated:
      - R   [* -> <r-version>]
      
      - Lockfile written to "<wd>/renv.lock".
      - The project is out-of-sync -- use `renv::status()` for details.

