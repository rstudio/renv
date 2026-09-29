
# Diagnostics for file.copy(copy.date = TRUE) on Windows, which opens
# the source file for exclusive access when reading its timestamp.

copy_repeatedly <- function(source, dates, n) {

  messages <- character()
  for (i in seq_len(n)) {

    target <- tempfile("copy-")
    dir.create(target)

    withCallingHandlers(
      file.copy(source, target, recursive = TRUE, copy.date = dates),
      warning = function(w) {
        messages <<- c(messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    unlink(target, recursive = TRUE)

  }

  messages

}

writeLines(R.version.string)

# 1. is the file time copied when the source file is open elsewhere?
src <- tempfile("source-")
dst <- tempfile("target-")
writeLines("hello", src)
Sys.setFileTime(src, as.POSIXct("2020-01-01 12:00:00", tz = "UTC"))

con <- file(src, "rb")
ok <- file.copy(src, dst, copy.date = TRUE)
close(con)

writeLines("")
writeLines("== copy with source file open in this process")
writeLines(sprintf("copied:       %s", ok))
writeLines(sprintf("source mtime: %s", format(file.mtime(src), tz = "UTC")))
writeLines(sprintf("target mtime: %s", format(file.mtime(dst), tz = "UTC")))

# 2. do concurrent copies of the same directory fail?
source <- tempfile("packages-")
dir.create(source)
for (i in 1:100)
  writeLines("hello", file.path(source, sprintf("file-%03i.txt", i)))

cl <- parallel::makeCluster(4L)
for (dates in c(TRUE, FALSE)) {

  results <- parallel::clusterCall(
    cl,
    copy_repeatedly,
    source = source,
    dates = dates,
    n = 100L
  )

  writeLines("")
  writeLines(sprintf("== 4 workers, 100 copies of 100 files each, copy.date = %s", dates))
  for (i in seq_along(results)) {
    messages <- results[[i]]
    writeLines(sprintf("worker %i: %i warning(s)", i, length(messages)))
    writeLines(sprintf("  %s", gsub("\\s+", " ", head(messages, 2L))))
  }

}

parallel::stopCluster(cl)
