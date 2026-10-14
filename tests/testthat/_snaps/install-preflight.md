# packages to be compiled from source are reported

    Code
      status <- renv_install_preflight_binaries_impl(records, rtools = TRUE)
    Output
      The following 2 packages will be compiled from source, as no binaries are available:
      - ragg [1.3.0]
      - stringi [1.8.4]
      

---

    Code
      status <- renv_install_preflight_binaries_impl(records, rtools = FALSE,
        version = "4.5.1")
    Output
      The following 2 packages will need to be compiled from source, as no binaries are available:
      - ragg [1.3.0]
      - stringi [1.8.4]
      
      The following 1 package will be built from source, and may need to be compiled:
      - mypkg [0.1.0]
      
      Compiling packages requires Rtools 4.5, which was not found.
      Call renv::equip() to install it, or download it from https://cran.r-project.org/bin/windows/Rtools/.
      

---

    Code
      status <- renv_install_preflight_binaries_impl(records["mypkg"], rtools = FALSE,
      version = "4.5.1")
    Output
      The following 1 package will be built from source, and may need to be compiled:
      - mypkg [0.1.0]
      
      Compiling packages requires Rtools 4.5, which was not found.
      Call renv::equip() to install it, or download it from https://cran.r-project.org/bin/windows/Rtools/.
      

