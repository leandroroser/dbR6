#' dbR6_save__
#'@keywords internal

dbR6_save  <- function(...) {
  with(parent.env(environment()), {
  to <- paste0(to, "sqlite")
  this_data <- super$get_where()$data
  if(this_data@dbname != ":memory:") stop("db already present on disk")
  db <- RSQLite::dbConnect(RSQLite::SQLite(), to)
  RSQLite::sqliteCopyDatabase(from = this_data, to = db)
  super$set_data(db)
  self$set_metadata()
  message("object saved on disk")
  invisible(self)
  })
}
