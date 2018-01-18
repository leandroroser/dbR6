#' dbR6_get_table__
#'@keywords internal

dbR6_get_table <- function(...) {

    cond1 <- !is.null(from) && from <= 0
    cond2 <- !is.null(to) && (to <= 0 || to > self$nrow(what))
    if (cond1 || cond2) {
        stop("from and to must be >= 1, and to may not exceed the number of rows of the table")
    }
    if (is.null(from) && !is.null(to)) {
        limits <- paste0(" LIMIT ", to)
    }
    else if (!is.null(from) && is.null(to)) {
        limits <- paste0(" LIMIT -1 OFFSET ", from - 1)
    }
    else if (!is.null(from) && !is.null(to)) {
        limits <- paste0(" LIMIT ", to - from + 1, " OFFSET ",
            from - 1)
    }
    else {
        limits <- ""
    }
    out <- self$send_query(paste0("SELECT * FROM ", what, limits))
    if (has_rownames) {
        out <- as_table_with_rownames(out)
    }
    out
}
