
#' get_table
#' @name get_table
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$get_table("mtcars_db")
#' my_db$get_table("mtcars_db", 1, 10)
#' }
#' @rdname get_table
#' @aliases get_table,dbR6
#' @exportMethod get_table

dbR6_get_table <- function(...) {

 #   table <- name_as_string(table) # object as string

    cond1 <- !is.null(start) && start <= 0
    cond2 <- !is.null(end) && (end <= 0 || end > self$nrow(tabname))
    if (cond1 || cond2) {
        stop("start and end must be >= 1, and end may not exceed the number of rows of the table")
    }
    if (is.null(start) && !is.null(end)) {
        limits <- paste0(" LIMIT ", end)
    } else if (!is.null(start) && is.null(end)) {
        limits <- paste0(" LIMIT -1 OFFSET ", start - 1)
    } else if (!is.null(start) && !is.null(end)) {
        limits <- paste0(" LIMIT ", end - start + 1, " OFFSET ",
            start - 1)
    } else {
        limits <- ""
    }
    out <- self$send_query(paste0("SELECT * FROM ", tabname, limits))
    if (has_rownames) {
        out <- as_table_with_rownames(out)
    }
    out
}
