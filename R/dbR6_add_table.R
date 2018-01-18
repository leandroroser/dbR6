#' dbR6_add_table
#'@keywords internal

dbR6_add_table <- function(...) {
  with(parent.env(environment()), {
  if(new_name %in% self$list_tables() && !overwrite && !append) {
    stop("The table ", new_name, " exists in the working directory. Use overwrite = TRUE to overwrite it")
  }
  names <- self$get_metadata()$df_names
  self$set_one_metadata_value("df_names", c(names, new_name))

  if(!is.null(fun)) {
    new_df <- fun(new_df)
  }

  RSQLite::dbWriteTable(super$get_where()$data, new_name, new_df, overwrite = overwrite,
                        append = append, row.names = write_rownames, ...)
  self$set_metadata()
  invisible(self)
  })
}
