
# used to bump the DESCRIPTION verison every time the package is rebuilt,
# just so that the renv cache entry is invalidated eagerly
local({

  if (!file.exists("internal"))
    return()

  lines <- readLines("DESCRIPTION")
  idx <- grep("^Version:", lines)
  line <- lines[[idx]]

  old <- numeric_version(sub("^Version:\\s+", "", line))

  parts <- unclass(old)[[1]]
  if (length(parts) < 4) {
    parts <- c(parts, rep(0, 4 - length(parts)))
  }

  parts[[4]] <- parts[[4]] + 1

  new <- paste(paste(parts[1:3], collapse = "."), parts[4], sep = "-")
  lines[[idx]] <- paste("Version:", new)
  writeLines(lines, "DESCRIPTION")

})
