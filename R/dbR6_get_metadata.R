
#' dbR6_get_metadata__
#'@keywords internal

dbR6_get_metadata = function() {
  with(parent.env(environment()), {
  private$metadata
  })
}
