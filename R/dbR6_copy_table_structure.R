#' dbR6_copy_table_structure__
#'@keywords internal

dbR6_copy_table_structure  <- function(...) {
  if(new_name %in% self$list_tables()) {
    if(!overwrite) {
      stop("The table ", to, " exists in the working directory. Use overwrite = TRUE to overwrite it")
    } else {
      self$remove_table(what = to)
    }
  }
  self$send_statement(paste0("CREATE TABLE ", to, " AS SELECT * FROM ", from, " WHERE 1=2"))
  invisible(self)
}
