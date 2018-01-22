#' dbR6_nrow__
#'@keywords internal

dbR6_nrow <- function(...) {
    if (!(table %in% self$list_tables()))
        stop(paste0("Table '", table, "' not found in database"))
    out <- self$send_query(paste0("SELECT COUNT(*) FROM ", table))
    out[[1]]
}
