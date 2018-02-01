#' dbR6_location
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$location()
#'
#' myfile <- tempfile()
#' my_db2 <- dbR6$new(myfile)
#' my_db2$location()
#'
#' }
#' @keywords internal

dbR6_location  <- function() {
    self$get_where()$data@dbname
}
