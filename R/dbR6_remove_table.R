#' dbR6_remove_table__
#'@keywords internal

dbR6_remove_table  <- function(...) {

  tables_names <- self$list_tables()
  for(this_table in what) {
    if(!(this_table %in% tables_names)) return(paste0("Table '", this_table,   "' not found in database"))
    RSQLite::dbRemoveTable(super$get_where()$data, this_table)
  }
  #this_statement <- RSQLite::dbSendStatement(super$get_where()$data,  paste0("DROP TABLE ", to_remove))
  #on.exit(RSQLite::dbClearResult(this_statement))
  self$set_metadata()
  invisible(self)
}

