
#' get_tables_number
#' @name get_tables_number
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$get_tables_number()
#' my_db$add_table(mtcars, "mtcars_db2")
#' my_db$get_tables_number()
#' }
#' @rdname get_tables_number
#' @aliases get_tables_number,dbR6
#' @exportMethod get_tables_number

dbR6_get_tables_number <- function() {
  out <- RSQLite::dbListTables(self$get_where()$data)
  length(out[out != "metadata"])
}
