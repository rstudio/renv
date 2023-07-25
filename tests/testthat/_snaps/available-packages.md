# available_packages() tolerates missing repositories

    Code
      dbs <- available_packages(type = "source")
    Output
      - Querying repositories for available source packages ... Done!
      renv was unable to query available packages from the following repositories:
      
      # <test-repo>/missing/src/contrib ---
      error downloading '<test-repo>/missing/src/contrib/PACKAGES.rds' [curl: (37) Couldn't open file <test-repo-path>/missing/src/contrib/PACKAGES.rds]
      error downloading '<test-repo>/missing/src/contrib/PACKAGES.gz' [curl: (37) Couldn't open file <test-repo-path>/missing/src/contrib/PACKAGES.gz]
      error downloading '<test-repo>/missing/src/contrib/PACKAGES' [curl: (37) Couldn't open file <test-repo-path>/missing/src/contrib/PACKAGES]
      

