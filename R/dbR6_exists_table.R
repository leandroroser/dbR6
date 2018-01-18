#' dbR6_exists_table
#'@keywords internal

dbR6_exists_table  <- function(...) {
  with(parent.env(environment()), {
  RSQLite::dbExistsTable(super$get_where()$data, what)
  })
}
