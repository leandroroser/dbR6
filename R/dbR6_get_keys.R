#' dbR6_get_keys__
#'@keywords internal

dbR6_get_keys  <- function() {
  with(parent.env(environment()), {
  private$keys
  })
}
