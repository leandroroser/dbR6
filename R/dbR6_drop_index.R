#' drop_index
#' @name drop_index
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$create_index("mtcars_db", column = "mpg", index_name = "mpg_index")
#' my_db$list_indices()
#' my_db$drop_index("mpg_index")
#' my_db$list_indices()
#' }
#' @rdname drop_index
#' @aliases drop_index,dbR6
#' @exportMethod drop_index


dbR6_drop_index <- function(...)  {
  self$send_statement(paste0("DROP INDEX IF EXISTS ", index))
  invisible(self)
}
