#' dbR6_nrow__
#'@keywords internal

dbR6_nrow <- function(...) {
  with(parent.env(environment()), {
    if (!(what %in% self$list_tables()))
        stop(paste0("Table '", what, "' not found in database"))
    out <- self$send_query(paste0("SELECT COUNT(*) FROM ", what))
    out[[1]]
  })
}
