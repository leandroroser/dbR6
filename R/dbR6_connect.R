self <-  NULL

#' dbR6_add_keys
#' @exportMethod connect

dbR6_connect  <- function(...) {
  dbR6$new(self$get_path()[[1]])
}

