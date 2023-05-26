# renv warns when installing an already-loaded package

    Code
      install("bread@0.1.0")
    Output
      Installing bread [0.1.0] ...
      	OK [copied cache in 22 milliseconds]
      Installed 1 package into library at path "/private/tmp/RtmpRjr7Nb/renv-library-a8a75e187b3d".
      The following package(s) have been updated:
      
      	bread [installed version 0.1.0 != loaded version 1.0.0]
      
      Consider restarting the R session and loading the newly-installed packages.
      

