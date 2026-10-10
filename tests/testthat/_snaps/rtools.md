# the toolchain preflight reports packages needing a source build

    Code
      status <- renv_rtools_preflight_impl(records, version = "4.5.1")
    Output
      The following package(s) are not available as binaries, and will be built from source:
      - ragg [1.3.0]
      - stringi [1.8.4]
      Packages containing compiled code require Rtools 4.5, which was not found.
      Install Rtools from https://cran.r-project.org/bin/windows/Rtools/, then restart R.
      

