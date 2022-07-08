
set -eux

: "${TMPDIR:=/tmp}"

if [ -z "${VERSION}" ]; then
  echo "Usage: VERSION=<version> make site"
  exit 0
fi

# Use our own fork so https://github.com/r-lib/pkgdown/issues/2152 doesn't break us
R -s -e 'renv::install("kevinushey/pkgdown@renv")'

cd "${TMPDIR}"

rm -rf renv-deploy
mkdir renv-deploy
cd renv-deploy

git clone -b "${VERSION}" https://github.com/rstudio/renv renv
git clone -b gh-pages https://github.com/rstudio/renv site

cd renv
R -s -e 'pkgdown::build_site()'
cd ..

cd site
rm -rf *
cd ..

cp -R renv/docs/ site/

cd site
rm reference/Rplot*
git add -A
git commit -m "Build site for renv: ${VERSION}"
git push -u
cd ..

cd ..
rm -rf renv-deploy
