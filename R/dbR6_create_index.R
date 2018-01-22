#' dbR6_create_index
#'@keywords internal

dbR6_create_index <- function(...) {
  if(index_name %in% self$get_index()) {
    stop("index name already exist")
  }
  dots <- as.character(unlist(list(...)))
  if(length(dots) > 0) column <- paste(c(column, dots), collapse = ", ")
  self$send_statement(paste0("CREATE ", ifelse(unique_index, "UNIQUE", ""),
                             " INDEX ", index_name, " ON ", table, " (", column, ")"))
  invisible(self)
}

