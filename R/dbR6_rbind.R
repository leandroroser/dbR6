#' dbR6_rbind
#'@keywords internal

dbR6_rbind  <- function(...) {
  if(grep(outname, data_on_disk$list_tables())) {
    if(!overwrite) {
      stop("the table ", to, " exists, but the parameter overwrite = FALSE")
    } else {
      data_on_disk$remove_table(to)
    }
  }

  union_type <- match.arg(union_type)
  table_list <- unlist(list(...))
  self$copy_table_structure(to, table_list[1])

  for(table_to_append in table_list) {
    self$send_statement(paste0("INSERT INTO ", to, " SELECT * FROM ", table_to_append))

    if(remove_after) {
      self$remove_table(table_to_append)
    }
  }
  invisible(self)
}
