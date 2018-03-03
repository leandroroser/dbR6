#' dbR6_map_reduce
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' fun <-  function(x) paste0("SELECT * FROM ", x, " WHERE ", paste0(x, ".mpg"), " > 20")
#' my_db$map_reduce(from = "mtcars_db", to = "map_reduce_result", column = "carb", query_function = fun)
#' my_db$list_tables()
#' my_db$get_table("map_reduce_result")
#' }
#' @keywords internal

dbR6_map_reduce <- function(...) {
  self$split(from = from, column = column, overwrite = overwrite, remove_after = remove_after)
  tables <- paste0(column, "_", private$keys[column][[1]])
  for(table_to_reduce in tables) {
    self$send_statement(paste0("CREATE TABLE __my_temp_table__ AS ", query_function(table_to_reduce)))
    self$remove_table(table_to_reduce)
    self$send_statement(paste0("ALTER TABLE __my_temp_table__ RENAME TO ", table_to_reduce))
  }
  if(is.null(to)) to <- paste0(from, "_", "map_reduced")
  self$reduce(from = column, to = to)
  private$set_metadata()
  invisible(self)
}
