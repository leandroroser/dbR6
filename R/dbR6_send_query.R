#' dbR6_send_query
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$send_query("SELECT COUNT(*) FROM mtcars_db")
#'
#' }
#' @keywords internal

dbR6_send_query  <- function(...) {
    if(as_tbl) {
      dplyr::tbl(self$get_connection(), dplyr::sql(query))
    } else {
    this_query <- RSQLite::dbSendQuery(self$get_where()$data, query)
    on.exit(RSQLite::dbClearResult(this_query))
    RSQLite::fetch(this_query)
    }
}
