#' dbR6_drop_index__
#'@keywords internal

dbR6_drop_index <- function(...)  {
  self$send_statement(paste0("DROP INDEX IF EXISTS ", index))
  invisible(self)
}
