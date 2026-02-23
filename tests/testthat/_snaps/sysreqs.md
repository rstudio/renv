# system requirements are reported as expected

    Code
      . <- renv_sysreqs_check(sysreqs, FALSE)
    Output
      The following required system packages are not installed:
      - blender  [required by <unknown>]
      The R packages depending on these system packages may fail to install.
      
      An administrator can install these packages with:
      - sudo apt install blender
      

# system requirements for alternate distributions are reported

    Code
      . <- sysreqs(packages, distro = "redhat:8")
    Output
      dnf install -y https://dl.fedoraproject.org/pub/epel/epel-release-latest-8.noarch.rpm
      subscription-manager repos --enable codeready-builder-for-rhel-8-$(arch)-rpms
      sudo dnf install -y ImageMagick-c++-devel
      sudo dnf install -y ImageMagick-devel
      sudo dnf install -y tesseract-devel

