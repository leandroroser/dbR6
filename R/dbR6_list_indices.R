
#' list_indices
#' @name list_indices
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$create_index("mtcars_db", column = "mpg", index_name = "mpg_index")
#' my_db$list_indices()
#' }
#' @rdname list_indices
#' @aliases list_indices,dbR6
#' @exportMethod list_indices

dbR6_list_indices <- function() {
  self$send_query("SELECT name FROM sqlite_master WHERE type='index' ORDER BY name;")
}
