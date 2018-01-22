#' dbR6_copy_table
#'@keywords internal

dbR6_copy_table  <- function(...) {

  if(to %in% self$list_tables()) {
    if(!overwrite) {
      stop("the table ", to, " exists, but the parameter overwrite = FALSE")
    } else {
      self$remove_table(to)
    }
  }

self$send_query(paste0("CREATE TABLE ", to, "AS SELECT * FROM ", from))
}
