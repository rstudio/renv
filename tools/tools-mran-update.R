
devtools::load_all()

options(
  renv.config.connect.timeout = 5L,
  renv.config.connect.retry = 0L
)

renv_mran_database_update(
  platform = "macosx/mavericks",
  version = "3.2"
)

renv_mran_database_update(
  platform = "macosx/mavericks",
  version = "3.3"
)

renv_mran_database_update(
  platform = "macosx/el-capitan",
  version = "3.4"
)

renv_mran_database_update(
  platform = "macosx/el-capitan",
  version = "3.5"
)

renv_mran_database_update(
  platform = "macosx/el-capitan",
  version = "3.6"
)
