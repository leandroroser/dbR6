#' dbR6_clone_db
#'@keywords internal

dbR6_clone_db  <- function(...)  {
  db <- RSQLite::dbConnect(RSQLite::SQLite(), to)
  RSQLite::sqliteCopyDatabase(from = super$get_where()$data, to = db)
  copy <- self$clone(deep = TRUE)
  copy$set_data(db)
  invisible(copy)
}

