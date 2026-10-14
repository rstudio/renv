# Install a build toolchain

Equip your system with the tools required to build R packages from
source. This is useful when a package recorded in the lockfile is not
available as a binary for your version of R, as can happen when
restoring an older project.

## Usage

``` r
equip()
```

## Value

This function is normally called for its side effects.

## Details

On Windows, `equip()` downloads and runs the installer for the version
of [Rtools](https://cran.r-project.org/bin/windows/Rtools/) matching the
running version of R. The installer may request administrator
privileges. (For R versions older than 4.0.0, `equip()` instead installs
the external libraries once used when building packages on Windows.)

On macOS, `equip()` ensures the Xcode command line tools are installed.

## Examples

``` r
if (FALSE) { # \dontrun{

# install a build toolchain, if one is not already available
renv::equip()

} # }
```
