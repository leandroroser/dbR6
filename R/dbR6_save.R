#' dbR6_list_tables
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' where <- tempfile()
#' my_db$save(where)
#' my_db$delete_db()
#' }
#' @keywords internal

dbR6_save  <- function(...) {
  to <- paste0(to, ".sqlite")
  this_data <- self$get_where()$data
  if(this_data@dbname != ":memory:") stop("db already present on disk")
  db <- RSQLite::dbConnect(RSQLite::SQLite(), to)
  RSQLite::sqliteCopyDatabase(from = this_data, to = db)
  self$set_data(db)
  private$set_metadata()
  message("object saved on disk")
  invisible(self)
}
