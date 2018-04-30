self <- private <- NULL

#' dbR6_add_keys
#'@keywords internal

dbR6_rename_table <- function(...) {
  self$send_statement(paste0("ALTER TABLE ", from, " RENAME TO ", to))
  index_names <- self$list_indices()
  from_index <- paste0(from, "_", "row_names")
  to_index <- paste0(to, "_", "row_names")
  if(from_index %in% index_names) {
    self$drop_index(from_index)
    self$create_index(to, column = "row_names",
      index_name = paste0(to, "_", "row_names"),
      overwrite = TRUE)
  }
  invisible(self)
}

