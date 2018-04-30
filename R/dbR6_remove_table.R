#' dbR6_remove_table
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$remove_table("mtcars_db")
#'
#' }
#' @keywords internal


dbR6_remove_table  <- function(...) {
  tables_names <- self$list_tables()
  index_names <- self$list_indices()

  for(this_table in tabname) {
    if(!(this_table %in% tables_names)) return(paste0("Table '",
      this_table,   "' not found in database"))

    RSQLite::dbRemoveTable(self$get_where()$data, this_table)

    # need to drop rownames index
    this_index <- paste0(this_table, "_", "row_names")
    if(this_index %in% index_names) self$drop_index(this_index)
  }
  #this_statement <- RSQLite::dbSendStatement(super$get_where()$data,
  # paste0("DROP TABLE ", to_remove))
  #on.exit(RSQLite::dbClearResult(this_statement))
  private$set_metadata()
  invisible(self)
}
