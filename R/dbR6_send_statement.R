#' dbR6_send_statement__
#'@keywords internal

dbR6_send_statement  <- function(...) {
  with(parent.env(environment()), {
    this_statement <- RSQLite::dbSendStatement(super$get_where()$data,
        statement)
    on.exit(RSQLite::dbClearResult(this_statement))
  })
}
