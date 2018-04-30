#' finalize
#' @name finalize
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$finalize()
#' }
#' @rdname finalize
#' @aliases finalize,dbR6
#' @exportMethod finalize

dbR6_finalize <- function() {
  super$finalize()
  private$validate_db()
}
