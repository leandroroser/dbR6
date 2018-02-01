#' dbR6_send_transaction__
#'@keywords internal

dbR6_send_transaction <- function(...) {
  # pass a list of unquoted arguments in transaction
  # example:
  # mylis <- list(a$transaction("CREATE TABLE t1(a, b PRIMARY KEY)"),
  # a$transaction("CREATE TABLE t2(a, b PRIMARY KEY)", "DROP TABLE t1"))
  # obj$transaction(mylist)
  fun <- function(...) {
    args <- as.list(substitute(...))[-1]
    x<-lapply(args, function(x) as.expression(bquote(RSQLite::dbExecute(self$get_where()$data, .(x)))))
    as.expression(unlist(x))
  }

  what <- fun(...)
  RSQLite::dbWithTransaction(self$get_where()$data, eval(what))
  TRUE
}

