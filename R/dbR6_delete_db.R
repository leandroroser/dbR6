
#' delete_db
#' @name delete_db
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$delete_db()
#' }
#' @rdname delete_db
#' @aliases delete_db,dbR6
#' @exportMethod delete_db


dbR6_delete_db <- function() {
  where <- self$get_where()$data@dbname
  if(where != ":memory:") {
    metadata_path <- self$get_path()[[2]]
    data_path <- self$location()
    self$initialize()
    self$finalize()
    file.remove(metadata_path)
    file.remove(data_path)
    cat("database removed from disk")
  } else {
    cat("database is in-memory, nothing to remove.")
  }
}
