
#' dim
#' @name dim
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$dim("mtcars_db")
#' }
#' @rdname dim
#' @aliases dim,dbR6
#' @exportMethod dim

dbR6_dim <- function(...) {
    c(self$nrow(tabname), self$ncol(tabname))
}
