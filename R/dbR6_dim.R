#' dbR6_dim__
#'@keywords internal

dbR6_dim <- function(...) {
  with(parent.env(environment()), {
    c(self$nrow(what), self$ncol(what))
  })
}
