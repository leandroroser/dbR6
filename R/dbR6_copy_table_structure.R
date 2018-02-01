#' dbR6_copy_table_structure
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$copy_table_structure("mtcars_db", "mtcars_db_copy")
#' my_db$get_table("mtcars_db_copy")
#' }
#' @keywords internal

dbR6_copy_table_structure  <- function(...) {
  if(to %in% self$list_tables()) {
    if(!overwrite) {
      stop("The table ", to, " exists in the working directory. Use overwrite = TRUE to overwrite it")
    } else {
      self$remove_table(tabname = to)
    }
  }
  self$send_statement(paste0("CREATE TABLE ", to, " AS SELECT * FROM ", from, " WHERE 1 = 2"))
  invisible(self)
}
