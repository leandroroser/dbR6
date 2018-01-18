#' dbR6_reduce__
#'@keywords internal

dbR6_reduce  <- function(...) {
  if(grep(what, data_on_disk$list_tables())) {
    if(!overwrite) {
      stop("some of output tables exist,", outname, " but the parameter overwrite = FALSE")
    } else {
      data_on_disk$remove_table(this_table)
    }
  }

  union_type <- match.arg(union_type)
  #tabnames <- self$list_tables()
  #which_tables <- tabnames[grep(what, tabnames)]
  which_tables <- (private$keys[what])[[1]]
  which_tables <- paste0(what, "_", which_tables)

  if(length(which_tables) == 0) {
    stop("name of variable do not exists. Check names with the method get_keys()")
  }

  self$rbind(outname = what, union_type = union_type, remove_after = remove_after, which_tables)
  self$remove_keys(what)
  self$set_metadata()
  invisible(self)
}
