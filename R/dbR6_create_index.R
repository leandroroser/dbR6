#' dbR6_create_index
#'@keywords internal

dbR6_create_index <- function(...) {
  dots <- as.character(unlist(list(...)))
  if(length(dots) > 0) column <- paste(c(column, dots), collapse = ", ")
  self$send_statement(paste0("CREATE ", ifelse(unique_index, "UNIQUE", ""),  " INDEX idx_temp ON ", what, " (", column, ")"))
  invisible(self)
}

