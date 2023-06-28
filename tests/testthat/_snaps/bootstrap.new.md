# bootstrapping gives informative output when succesful

    Code
      bootstrap(version = "0.9.0", library = library)
    Output
      # Bootstrapping renv 0.9.0 ---
      - Downloading renv ... OK
      - Installing renv  ... OK
      
    Code
      bootstrap(version = "1.0.0", library = library)
    Output
      # Bootstrapping renv 1.0.0 ---
      - Downloading renv ... OK
      - Installing renv  ... OK
      
    Code
      bootstrap(version = "1.0.0.1", library = library)
    Output
      # Bootstrapping renv 1.0.0.1 ---
      - Downloading renv ... OK
      - Installing renv  ... OK
      

# bootstrapping gives informative output when download fails

    Code
      bootstrap(version = "0.9.0", library = library)
    Output
      # Bootstrapping renv 0.9.0 ---
      - Downloading renv ... FAILED
    Error <simpleError>
      failed to download:
      All download methods failed
    Code
      bootstrap(version = "1.0.0", library = library)
    Output
      # Bootstrapping renv 1.0.0 ---
      - Downloading renv ... FAILED
    Error <simpleError>
      failed to download:
      All download methods failed
    Code
      bootstrap(version = "1.0.0.1", library = library)
    Output
      # Bootstrapping renv 1.0.0.1 ---
      - Downloading renv ... FAILED
    Error <simpleError>
      failed to download:
      All download methods failed

# bootstrapping gives informative output when install fails

    Code
      bootstrap(version = "1.0.0.1", library = library)
    Output
      # Bootstrapping renv 1.0.0.1 ---
      - Downloading renv ... OK
      - Installing renv  ... FAILED
    Error <simpleError>
      failed to install:
      installation of renv failed
      ===========================
      test failure

# renv_boostrap_version_validate() gives good warnings

    Code
      . <- renv_bootstrap_validate_version("1.2.3", list(Version = "2.3.4"))
    Output
      renv 2.3.4 was loaded from project library, but this project is configured to use renv 1.2.3.
      * Use `renv::record("renv@2.3.4")` to record renv 2.3.4 in the lockfile.
      * Use `renv::restore(packages = "renv")` to install renv 1.2.3 into the project library.
    Code
      . <- renv_bootstrap_validate_version("1.2.3-4", list(Version = "1.2.3"))
    Output
      renv 1.2.3 was loaded from project library, but this project is configured to use renv 1.2.3-4.
      * Use `renv::record("renv@1.2.3")` to record renv 1.2.3 in the lockfile.
      * Use `renv::restore(packages = "renv")` to install renv 1.2.3-4 into the project library.
    Code
      . <- renv_bootstrap_validate_version(version = "22d015905828c3398728a5ff9e381e0433976263",
        description = list(RemoteSha = "6b09befaaba3f55e0e2c141cb45c5d247b61ef1e",
          Version = "1.2.3"))
    Output
      renv 1.2.3 [sha: 6b09bef] was loaded from project library, but this project is configured to use renv [sha: 22d0159].
      * Use `renv::record("rstudio/renv@6b09befaaba3f55e0e2c141cb45c5d247b61ef1e")` to record renv 1.2.3 [sha: 6b09bef] in the lockfile.
      * Use `renv::restore(packages = "renv")` to install renv [sha: 22d0159] into the project library.
    Code
      . <- renv_bootstrap_validate_version(version = "22d015905828c3398728a5ff9e381e0433976263",
        description = list(Version = "1.2.3"))
    Output
      renv 1.2.3 was loaded from project library, but this project is configured to use renv [sha: 22d0159].
      * Use `renv::record("renv@1.2.3")` to record renv 1.2.3 in the lockfile.
      * Use `renv::restore(packages = "renv")` to install renv [sha: 22d0159] into the project library.

