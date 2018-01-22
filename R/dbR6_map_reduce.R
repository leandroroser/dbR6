#' dbR6_map_reduce__
#'@keywords internal

dbR6_map_reduce <- function(...) {
  self$split(from, column, overwrite = overwrite, remove_after = remove_after)
  for(table_to_reduce in self$keys[column]) {
    self$send_query("CREATE TABLE __my_temp_table__ AS", query_function(table_to_reduce))
    self$remove_table(table_to_reduce)
    self$send_statement("ALTER TABLE __my_temp_table__ RENAME TO ", table_to_reduce)
  }
  self$reduce(table_to_reduce, column, "union")
  self$set_metadata()
  invisible(self)
}
