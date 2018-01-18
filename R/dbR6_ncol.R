#' dbR6_ncol__
#'@keywords internal

dbR6_ncol  <- function(...) {
    if (!what %in% self$list_tables())
        stop(paste0("Table '", what, "' not found in database"))
    out <- RSQLite::dbListFields(super$get_where()$data, what)
    length(out)
}
