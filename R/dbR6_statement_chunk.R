#' dbR6_statement_chunk__
#'@keywords internal

dbR6_statement_chunk  <- function(...)  {
  t_s <- RSQLite::dbSendStatement(super$get_where()$data, what)
  while (!RSQLite::dbHasCompleted(t_s)) {
    chunk <- RSQLite::dbFetch(t_s, n = n)
    print(nrow(chunk))
  }
  RSQLite::dbClearResult(t_s)
  invisible(self)
}
