# bootstrapping gives informative output when succesful

    Code
      bootstrap(version = "0.9.0", library = library)
    Output
      Bootstrapping renv 0.9.0:
      * Downloading binary from <https://cran.rstudio.com> ... OK
      * Installing ... OK
      
      
    Code
      bootstrap(version = "1.0.0", library = library)
    Output
      Bootstrapping renv 1.0.0:
      * Downloading from archive ... OK
      * Installing ... OK
      
      
    Code
      bootstrap(version = "1.0.0.1", library = library)
    Output
      Bootstrapping renv 1.0.0.1:
      * Downloading version 1.0.0.1 from GitHub ... OK
      * Installing ... OK
      
      

# bootstrapping gives informative output when download fails

    Code
      bootstrap(version = "0.9.0", library = library)
    Output
      Bootstrapping renv 0.9.0:
      * Downloading binary from <https://cran.rstudio.com> ... FAILED
      * Downloading from archive ... FAILED
    Error <simpleError>
      failed to download:
      All download methods failed
    Code
      bootstrap(version = "1.0.0", library = library)
    Output
      Bootstrapping renv 1.0.0:
      * Downloading from archive ... FAILED
    Error <simpleError>
      failed to download:
      All download methods failed
    Code
      bootstrap(version = "1.0.0.1", library = library)
    Output
      Bootstrapping renv 1.0.0.1:
      * Downloading version 1.0.0.1 from GitHub ... FAILED
    Error <simpleError>
      failed to download:
      All download methods failed

# bootstrapping gives informative output when install fails

    Code
      bootstrap(version = "1.0.0.1", library = library)
    Output
      Bootstrapping renv 1.0.0.1:
      * Downloading version 1.0.0.1 from GitHub ... OK
      * Installing ... FAILED
      Error installing renv:
      ======================
      test failure
    Error <simpleError>
      failed to install:
      Failed

# renv_boostrap_version_validate() gives good warnings

    Code
      . <- renv_bootstrap_validate_version("abcd", list(RemoteSha = "efgh"))
    Output
      renv efgh was loaded from project library, but this project is configured to use renv abcd.
      * Use `renv::record("rstudio/renv@efgh")` to record renv efgh in the lockfile.
      * Use `renv::restore(packages = "renv")` to install renv abcd into the project library.
    Code
      . <- renv_bootstrap_validate_version("1.2.3", list(Version = "2.3.4"))
    Output
      renv 2.3.4 was loaded from project library, but this project is configured to use renv 1.2.3.
      * Use `renv::record("renv@2.3.4")` to record renv 2.3.4 in the lockfile.
      * Use `renv::restore(packages = "renv")` to install renv 1.2.3 into the project library.

