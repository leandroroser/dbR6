#' dbR6_send_statement__
#'@keywords internal

dbR6_send_statement  <- function(...) {
    this_statement <- RSQLite::dbSendStatement(super$get_where()$data,
        statement)
    on.exit(RSQLite::dbClearResult(this_statement))
}
