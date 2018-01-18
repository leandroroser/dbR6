#' dbR6_get_tables_number__
#'@keywords internal

dbR6_get_tables_number <- function(...) {
    out <- RSQLite::dbListTables(super$get_where()$data)
}
