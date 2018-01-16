
#' dbR6_deep_clone__
#'@keywords internal

dbR6_deep_clone <- function(...) {
  # With x$clone(deep=TRUE) is called, the deep_clone gets invoked once for
  # each field, with the name and value.
  head_env <- new.env(parent = emptyenv(), hash = FALSE)
  if (name == "where") {
    list2env(as.list.environment(value, all.names = TRUE), envir = head_env)
  } else if(name == "metadata") {
    list2env(as.list.environment(value, all.names = TRUE), parent = head_env, hash = FALSE)
  } else {
    # For all other fields, just return the value
    value
  }
}
