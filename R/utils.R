## functions taken from vimc/orderly/util.R
is_directory <- function(x) {
  file.info(x, extra_cols = FALSE)$isdir
}

rbind_df <- function(x) {
  do.call("rbind", x)
}

squote <- function(x) {
  sprintf("'%s'", x)
}

dquote <- function(x) {
  sprintf('"%s"', x)
}

pasteq <- function(x, sep = ", ") {
  paste(squote(x), collapse = ", ")
}

list_depth <- function(this)
    ifelse(is.list(this), 1L + max(sapply(this, list_depth)), 0L)
