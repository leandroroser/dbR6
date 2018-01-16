#' dbR6_dim__
#'@keywords internal

dbR6_dim <- function(...) {
    c(self$nrow(what), self$ncol(what))
}
