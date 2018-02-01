#' dbR6_create_index
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$create_index("mtcars_db", column = "mpg", index_name = "mpg_index")
#' my_db$list_indices()
#' }
#' @keywords internal

dbR6_create_index <- function(...) {
  if(index_name %in% self$list_indices()) {
    stop("index name already exist")
  }
  dots <- as.character(unlist(list(...)))
  if(length(dots) > 0) column <- paste(c(column, dots), collapse = ", ")
  self$send_statement(paste0("CREATE ", ifelse(unique_index, "UNIQUE", ""),
                             " INDEX ", index_name, " ON ", tabname, " (", column, ")"))
  invisible(self)
}

