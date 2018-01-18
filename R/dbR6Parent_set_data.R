#' dbR6Parent_set_data__
#'@keywords internal

dbR6Parent_set_data <- function(...) {
  with(parent.env(environment()), {
  private$where$data <- x
  })
}
