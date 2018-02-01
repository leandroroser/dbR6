#' dbR6_exists_table
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$exists_table("mtcars_db")
#' my_db$exists_table("hello")
#' }
#' @keywords internal

dbR6_exists_table  <- function(...) {
  RSQLite::dbExistsTable(self$get_where()$data, tabname)
}
