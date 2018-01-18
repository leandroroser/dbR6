#' dbR6_get_index__
#'@keywords internal

dbR6_get_index  <- function() {
  with(parent.env(environment()), {
  self$send_query("SELECT name FROM sqlite_master WHERE type='index' ORDER BY name;")
  })
}
