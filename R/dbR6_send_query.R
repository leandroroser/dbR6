#' dbR6_send_query__
#'@keywords internal

dbR6_send_query  <- function(...) {
  with(parent.env(environment()), {
    this_query <- RSQLite::dbSendQuery(super$get_where()$data,
        query)
    on.exit(RSQLite::dbClearResult(this_query))
  })
}