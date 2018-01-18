#' dbR6Parent_get_where__
#'@keywords internal

dbR6Parent_get_where <- function() {
  with(parent.env(environment()), {
  private$where
  })
}
