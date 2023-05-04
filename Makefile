.PHONY: config tags

bump:
	@tools/tools-bump.sh

config: R/config-defaults.R

release:
	@tools/tools-release.sh

site:
	@tools/tools-build-site.sh

tags:
	@Rscript tools/tools-git-tags.R
	@git pull

R/config-defaults.R: inst/config.yml
	@Rscript tools/tools-generate-config.R

