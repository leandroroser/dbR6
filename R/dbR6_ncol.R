#' dbR6_ncol__
#'@keywords internal

dbR6_ncol  <- function(...) {
    if (!table %in% self$list_tables())
        stop(paste0("Table '", table, "' not found in database"))
    out <- RSQLite::dbListFields(super$get_where()$data, table)
    length(out)
}
