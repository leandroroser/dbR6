#' dbR6_copy_table_structure__
#'@keywords internal

dbR6_copy_table_structure  <- function(...) {
  if(new_name %in% self$list_tables()) {
    if(!overwrite) {
      stop("The table ", new_name, " exists in the working directory. Use overwrite = TRUE to overwrite it")
    } else {
      self$remove_table(what = new_name)
    }
  }
  self$send_statement(paste0("CREATE TABLE ", new_name, " AS SELECT * FROM ", from, " WHERE 1=2"))
  invisible(self)
}
