#' dbR6_colnames__
#'@keywords internal

dbR6_colnames  <- function(...) {
    if (!table %in% self$list_tables())
        return(paste0("Table '", table, "' not found in database"))
    RSQLite::dbListFields(super$get_where()$data, table)
}

