#' dbR6_add_keys
#'@keywords internal

dbR6_add_keys  <- function(...) {
  with(parent.env(environment()), {
  private$keys[key] = list(value)
  self$set_metadata()
  })
}

