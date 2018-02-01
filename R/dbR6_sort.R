#' dbR6_sort
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$get_table("mtcars_db", 1, 10)
#' my_db$sort("mtcars_db", "cyl")
#' my_db$get_table("mtcars_db", 1, 10)
#' my_db$sort("mtcars_db", "cyl", "disp", "vs")
#' my_db$get_table("mtcars_db", 1, 10)
#' }
#' @keywords internal

dbR6_sort  <- function(...)   {
  dots <- as.character(unlist(list(...)))
  if(length(dots) > 0) column <- paste(c(column, dots), collapse = ", ")

  # > cost of time (create index before?)
  #self$send_statement(paste0("CREATE INDEX idx_temp ON ", tabname, " (", column, ")"))

  tempname <- paste("temp_", paste(sample(c(letters, 0:9, 20)), collapse = ""), sep = "")
  self$copy_table_structure(tabname, tempname, overwrite = TRUE)
  self$send_statement(paste0("INSERT INTO ", tempname, " SELECT * FROM ", tabname, " ORDER BY ", column))
  self$remove_table(tabname)
  self$send_statement(paste0("ALTER TABLE ", tempname, " RENAME TO ", tabname))

  #self$send_statement("DROP INDEX IF EXISTS idx_temp") # but is previously removed when drop

  invisible(self)
}
