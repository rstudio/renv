# bootstrapping gives informative output when succesful

    Code
      bootstrap(version = "0.9.0", library = library)
    Output
      Bootstrapping renv 0.9.0:
      * Downloading binary from CRAN ... OK
      * Installing ... OK
      
      
    Code
      bootstrap(version = "1.0.0", library = library)
    Output
      Bootstrapping renv 1.0.0:
      * Downloading from CRAN archive ... OK
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
      * Downloading binary from CRAN ... FAILED
      * Downloading from CRAN archive ... FAILED
    Error <simpleError>
      Failed to download 
    Code
      bootstrap(version = "1.0.0", library = library)
    Output
      Bootstrapping renv 1.0.0:
      * Downloading from CRAN archive ... FAILED
    Error <simpleError>
      Failed to download 
    Code
      bootstrap(version = "1.0.0.1", library = library)
    Output
      Bootstrapping renv 1.0.0.1:
      * Downloading from GitHub ... FAILED
    Error <simpleError>
      Failed to download 

# bootstrapping gives informative output when install fails

    Code
      bootstrap(version = "1.0.0.1", library = library)
    Output
      Bootstrapping renv 1.0.0.1:
      * Downloading from GitHub ... OK
      * Installing ... FAILED
      Error installing renv:
      ======================
      Failed to install
    Error <simpleError>
      Failed to install

