#' dbR6_send_query_chunk
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$copy_table_structure("mtcars_db", "out")
#' my_query <- "SELECT  * FROM mtcars_db WHERE carb = 2"
#' out <- my_db$send_query_chunk("mtcars_db", query=my_query, n  = 3)
#' }
#' @keywords internal

dbR6_send_query_chunk  <- function(...) {
  t_s <- RSQLite::dbSendQuery(self$get_where()$data, query)
  n_processed <- 0
  while (!RSQLite::dbHasCompleted(t_s)) {
    if(n_processed == 0) {
      out <- RSQLite::dbFetch(t_s, n = n)
    } else {
      chunk <- RSQLite::dbFetch(t_s, n = n)
      if(nrow(chunk != 0)) {
        out <- rbind(out, chunk)
      }
    }
    n_processed <- n_processed + n
    cat(paste0("Yield ", n_processed , "\n"))
  }
  RSQLite::dbClearResult(t_s)
  out
}
