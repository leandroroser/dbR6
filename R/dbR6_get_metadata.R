
#' get_metadata
#' @name get_metadata
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$get_metadata()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$get_metadata()
#' }
#' @rdname get_metadata
#' @aliases get_metadata,dbR6
#' @exportMethod get_metadata

dbR6_get_metadata = function() {
  as.list(private$metadata)
}
