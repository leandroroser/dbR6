#' dbR6_colnames
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$get_tables_number()
#' my_db$add_table(mtcars, "mtcars_db2")
#' my_db$get_tables_number()
#' }
#' @keywords internal

dbR6_get_tables_number <- function() {
  out <- RSQLite::dbListTables(self$get_where()$data)
  length(out[out != "metadata"])
}
