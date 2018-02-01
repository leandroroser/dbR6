#' dbR6_send_query
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$list_tables()
#' my_db$send_query("SELECT COUNT(*) FROM mtcars_db")
#' my_db$send_statement("DROP TABLE mtcars_db")
#' my_db$list_tables()
#' }
#' @keywords internal

dbR6_send_statement  <- function(...) {
    this_statement <- RSQLite::dbSendStatement(self$get_where()$data, statement)
    on.exit(RSQLite::dbClearResult(this_statement))
    TRUE
}
