# repair() can update DESCRIPTION files for GitHub packages

    Code
      . <- repair()
    Output
      # Library cache links ---
      - No issues found with the project library's cache links.
      
      # Package sources ---
      The following package(s) do not have an explicitly-declared remote source.
      However, renv was available to infer remote sources from their DESCRIPTION file.
      - skeleton  [kevinushey/skeleton]
      `renv::restore()` may fail for packages without an explicitly-declared remote source.
      
      What would you like to do?
      
      1: Let renv infer the remote sources for these packages.
      2: Do nothing and resolve the situation another way.
      
      Selection: 2
      
    Condition
      Error:
      ! Operation canceled

