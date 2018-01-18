#' dbR6_get_tables_number__
#'@keywords internal

dbR6_get_tables_number <- function() {
  out <- RSQLite::dbListTables(self$get_where()$data)
  length(out[out != "metadata"])
}
