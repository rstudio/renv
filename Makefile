.PHONY: config tags

config: R/config-defaults.R

tags:
	@Rscript tools/tools-git-tags.R
	@git pull

R/config-defaults.R: inst/config.yml
	@Rscript tools/tools-generate-config.R

