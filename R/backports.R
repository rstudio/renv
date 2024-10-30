
if (is.null(.BaseNamespaceEnv$startsWith)) {
  
  startsWith <- function(x, prefix) {
    pattern <- sprintf("^\\Q%s\\E", prefix)
    grepl(pattern, x, perl = TRUE)
  }
  
}

if (is.null(.BaseNamespaceEnv$lengths)) {

  lengths <- function(x, use.names = TRUE) {
    vapply(x, length, numeric(1), USE.NAMES = use.names)
  }

}
