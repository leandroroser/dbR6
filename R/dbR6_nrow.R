#' dbR6_nrow
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$nrow("mtcars_db")
#' }
#' @keywords internal

dbR6_nrow <- function(...) {
    if (!(tabname %in% self$list_tables()))
        stop(paste0("Table '", tabname, "' not found in database"))
    out <- self$send_query(paste0("SELECT COUNT(*) FROM ", tabname))
    out[[1]]
}
