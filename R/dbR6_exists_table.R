#' exists_table
#' @name exists_table
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$exists_table("mtcars_db")
#' my_db$exists_table("hello")
#' }
#' @rdname exists_table
#' @aliases exists,dbR6
#' @exportMethod exists_table

dbR6_exists_table  <- function(...) {
  RSQLite::dbExistsTable(self$get_where()$data, tabname)
}
