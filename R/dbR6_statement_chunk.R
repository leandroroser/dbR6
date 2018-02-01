#' dbR6_statement_chunk
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#'
#' # COMPLETAR---
#' }
#' @keywords internal

dbR6_statement_chunk  <- function(...) {
  t_s <- RSQLite::dbSendStatement(self$get_where()$data, statement)
  while (!RSQLite::dbHasCompleted(t_s)) {
    chunk <- RSQLite::dbFetch(t_s, n = n)
    print(nrow(chunk))
  }
  RSQLite::dbClearResult(t_s)
  invisible(self)
}
