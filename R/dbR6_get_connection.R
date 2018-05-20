#' add_table
#' @name get_connection
#' @description get sqlite connection
#' @examples
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' }
#' @rdname get_connection
#' @aliases get_connection,dbR6
#' @exportMethod get_connection

dbR6_get_connection <- function(...) {
  private$where$data
}
