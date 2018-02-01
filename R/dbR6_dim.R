#' dbR6_dim
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$copy_table("mtcars_db", "mtcars_db_copy")
#' }
#' @keywords internal

dbR6_dim <- function(...) {
    c(self$nrow(tabname), self$ncol(tabname))
}
