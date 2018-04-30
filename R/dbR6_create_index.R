#' create_index
#' @name create_index
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$create_index("mtcars_db", column = "mpg", index_name = "mpg_index")
#' my_db$list_indices()
#' }
#' @rdname create_index
#' @aliases create_index,dbR6
#' @exportMethod create_index

dbR6_create_index <- function(...) {
  if(index_name %in% self$list_indices()) {
    if(!overwrite) {
    stop("index name already exist")
    } else {
     self$drop_index(index_name)
    }
  }
  dots <- as.character(unlist(list(...)))
  if(length(dots) > 0) column <- paste(c(column, dots), collapse = ", ")
  self$send_statement(paste0("CREATE ", ifelse(unique_index, "UNIQUE", ""),
                             " INDEX ", index_name, " ON ", tabname, " (", column, ")"))
  invisible(self)
}

