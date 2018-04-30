#' is_valid
#' @name is_valid
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$is_valid()
#' my_db$finalize()
#' my_db$is_valid()
#' }
#' @rdname is_valid
#' @aliases is_valid,dbR6
#' @exportMethod is_valid

dbR6_is_valid <- function() {
  private$valid
}
