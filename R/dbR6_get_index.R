#' dbR6_get_index__
#'@keywords internal

dbR6_get_index  <- function() {
  self$send_query("SELECT name FROM sqlite_master WHERE type='index' ORDER BY name;")
}
