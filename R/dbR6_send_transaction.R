#' dbR6_send_transaction
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' mylist <- list("CREATE TABLE t1(row_names PRIMARY KEY, name, surname, position)",
#'               "INSERT INTO t1(row_names, name, surname, position) VALUES (10, 'Diego', 'Maradona', 1)",
#'               "CREATE TABLE mt2 AS SELECT * FROM mtcars_db")
#' my_db$send_transaction(mylist)
#' my_db$get_table("t1")
#' }
#'@keywords internal

dbR6_send_transaction <- function(...) {
  create_query_expression <- function(...) {
    #args <- as.list(substitute(...))[-1]
    args <- as.list(...)
    x<-lapply(args, function(x) as.expression(bquote(RSQLite::dbExecute(self$get_where()$data, .(x)))))
    as.expression(unlist(x))
  }
  what <- create_query_expression(...)
  RSQLite::dbWithTransaction(self$get_where()$data, eval(what))
  TRUE
}

