#' dbR6_ncol
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$ncol("mtcars_db")
#' }
#' @keywords internal

dbR6_ncol  <- function(...) {
    if (!tabname %in% self$list_tables())
        stop(paste0("Table '", tabname, "' not found in database"))
    out <- RSQLite::dbListFields(super$get_where()$data, tabname)
    length(out)
}
