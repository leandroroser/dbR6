#' dbR6_get_metadata
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$get_metadata()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$get_metadata()
#' }
#' @keywords internal

dbR6_get_metadata = function() {
  as.list(private$metadata)
}
