#' dbR6_list_tables
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$list_tables()
#' }
#' @keywords internal

dbR6_list_tables = function() {
  out <- RSQLite::dbListTables(self$get_where()$data)
  out <- out[out != "metadata"]
  if(length(out) == 0)  {
    return("")
  } else return(sort(out))
}
