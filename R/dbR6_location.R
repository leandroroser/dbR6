#' dbR6_location__
#'@keywords internal

dbR6_location  <- function() {
  with(parent.env(environment()), {
    super$get_where()$data@dbname
  })
}
