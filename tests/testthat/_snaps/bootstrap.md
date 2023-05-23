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
      * Downloading from GitHub ... OK
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
      * Downloading from GitHub ... FAILED
    Error <simpleError>
      failed to download:
      All download methods failed

# bootstrapping gives informative output when install fails

    Code
      bootstrap(version = "1.0.0.1", library = library)
    Output
      Bootstrapping renv 1.0.0.1:
      * Downloading from GitHub ... OK
      * Installing ... FAILED
      Error installing renv:
      ======================
      test failure
    Error <simpleError>
      failed to install:
      Failed

