#' dbR6_remove_keys__
#'@keywords internal

dbR6_remove_keys  <- function(...) {
  which_key <- which(names(self$keys) == key)
  if(length(which_key)>0) self$keys <- self$keys[-which_key]
  self$set_metadata()
}
