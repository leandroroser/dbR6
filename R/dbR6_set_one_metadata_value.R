
#' dbR6_set_one_metadata_value__
#'@keywords internal

dbR6_set_one_metadata_value = function(...) {
  with(parent.env(environment()), {
    name <- deparse(substitute(name))
    assign(name, value, private$metadata)
  })
  }
