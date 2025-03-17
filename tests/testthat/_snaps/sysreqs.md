# system requirements are reported as expected

    Code
      . <- renv_sysreqs_check(sysreqs, FALSE)
    Output
      The following required system packages are not installed:
      - blender  [required by <unknown>]
      The R packages depending on these system packages may fail to install.
      
      An administrator can install these packages with:
      - sudo apt install blender
      

