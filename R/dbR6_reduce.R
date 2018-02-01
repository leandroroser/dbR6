#' dbR6_reduce
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$split("mtcars_db", "splitted", "gear")
#' my_db$list_tables()
#' my_db$reduce("gear", "reduced")
#' my_db$list_tables()
#' }
#' @keywords internal

dbR6_reduce  <- function(...) {
  if(length(grep(to, my_db$list_tables())) > 0) {
    if(!overwrite) {
      stop("some of output tables exist,", to, " but the parameter overwrite = FALSE")
    } else {
      self$remove_table(to)
    }
  }

  which_tables <- (private$keys[from])[[1]]
  which_tables <- sort(paste0(from, "_", which_tables[order(which_tables)]))


  if(length(which_tables) == 0)
    stop("name of variable do not exists. Check names with the method get_keys()")

  self$rbind(to, which_tables, remove_appended = "sequential")
  private$remove_keys(from)
  private$set_metadata()
  invisible(self)
}
