#' dbR6_clone_db
#' @example
#' {
#' data(mtcars2)
#' tmp <- tempfile()
#' write.table(mtcars, tmp, quote=FALSE)
#' my_db <- dbR6$new("original_file")
#' my_db$add_table(mtcars, "mtcarsdb")
#' new_db <- my_db$clone_db("cloned_in_disk")
#' }
#' @keywords internal

dbR6_clone_db  <- function(...)  {

  # automatically add file type
  if(to != ":memory:") {
    if(length(grep(".sqlite$", to, ignore.case = TRUE)) == 0) {
    to <-  normalizePath(paste0(to, ".sqlite"))
    }
  }

  db <- RSQLite::dbConnect(RSQLite::SQLite(), to)
  RSQLite::sqliteCopyDatabase(from = self$get_where()$data, to = db)
  copy <- self$clone(deep = TRUE)
  copy$set_data(db)
  invisible(copy)
}

