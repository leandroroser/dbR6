#' dbR6_copy_table
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$copy_table("mtcars_db", "mtcars_db_copy")
#' }
#' @keywords internal

dbR6_copy_table  <- function(...) {

  if(to %in% self$list_tables()) {
    if(!overwrite) {
      stop("the table ", to, " exists, but the parameter overwrite = FALSE")
    } else {
      self$remove_table(to)
    }
  }

self$send_statement(paste0("CREATE TABLE ", to, " AS SELECT * FROM ", from))
}
