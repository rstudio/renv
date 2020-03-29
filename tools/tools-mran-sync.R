
devtools::load_all()

updated <- any(
  renv_mran_database_sync("windows", "3.6"),
  renv_mran_database_sync("macosx/el-capitan", "3.6")
)

if (updated) {

  source <- renv_mran_database_path()
  target <- "s3://rstudio-buildtools/renv/mran/packages.rds"

  args <- c("s3", "cp", shQuote(source), shQuote(target))
  system2("aws", args)

}
