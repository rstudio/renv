# init() prompts the user for the snapshot type

    Code
      init()
    Output
      This project contains a DESCRIPTION file.
      Which files should renv use for dependency discovery in this project?
      
      1: Use only the DESCRIPTION file. (explicit mode)
      2: Use all files in this project. (implicit mode)
      
      Selection: 1
      
      - Using 'explicit' snapshot type. Please see `?renv::snapshot` for more details.
      
      - Resolving missing dependencies ... 
      # Installing packages ---
      
      - Installing bread ...                          OK [linked from cache in XXs]
      
      The following package(s) will be updated in the lockfile:
      
      # CRAN ---
      - bread   [* -> 1.0.0]
      
      The version of R recorded in the lockfile will be updated:
      - R       [* -> <r-version>]
      
      - Lockfile written to '<wd>/renv.lock'.

