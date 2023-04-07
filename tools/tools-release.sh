#!/usr/bin/env sh

set -e

_exit () {

  cd "${OWD}"
  if [ -n "${OK}" ]; then
    rm -f DESCRIPTION.backup
    rm -f NEWS.md.backup
  else
    mv DESCRIPTION.backup DESCRIPTION
    mv NEWS.md.backup NEWS.md
  fi

}

trap _exit EXIT

OWD="$(pwd)"
if [ -z "${VERSION}" ]; then
  echo "Usage: VERSION=<version> make release"
  exit 1
fi

# check that the working tree is not dirty
if [ -n "$(git diff --stat)" ]; then
  echo "ERROR: 'git diff --stat' reports that the repository contains uncommitted changed; cannot proceed"
  exit 1
fi

# update the version in the DESCRIPTION
cp DESCRIPTION DESCRIPTION.backup
sed "s|Version:.*|Version: ${VERSION}|g" DESCRIPTION > DESCRIPTION.tmp
mv DESCRIPTION.tmp DESCRIPTION

# remove UNRELEASED from NEWS
cp NEWS.md NEWS.md.backup
sed "s|.*(UNRELEASED)|# renv ${VERSION}|g" NEWS.md > NEWS.md.tmp
mv NEWS.md.tmp NEWS.md

# remove inst/doc (these will be rebuilt)
rm -rf inst/doc

# preform other pre-flight checks
R -e 'devtools::load_all(); renv:::renv_release_preflight()'

# build and check the package
cd ..
R CMD build renv
R CMD check "renv_${VERSION}.tar.gz"
cd renv

# set OK flag for exit handler
OK=1

