#' dbR6_colnames__
#'@keywords internal

dbR6_colnames  <- function(...) {
    if (!what %in% self$list_tables())
        return(paste0("Table '", what, "' not found in database"))
    RSQLite::dbListFields(super$get_where()$data, what)
}

