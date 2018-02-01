#' dbR6_colnames
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$colnames("mtcars_db")
#' }
#' @keywords internal

dbR6_colnames  <- function(...) {
    if (!tabname %in% self$list_tables())
        return(paste0("Table '", tabname, "' not found in database"))
    RSQLite::dbListFields(self$get_where()$data, tabname)
}

