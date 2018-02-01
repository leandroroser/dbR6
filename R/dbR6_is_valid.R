#' dbR6_is_valid
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$is_valid()
#' my_db$finalize()
#' my_db$is_valid()
#' }
#' @export

dbR6_is_valid <- function() {
  private$valid
}
