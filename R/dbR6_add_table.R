#' dbR6_add_table
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' }
#' @keywords internal

dbR6_add_table <- function(...) {

  if(to %in% self$list_tables() && !overwrite && !append) {
    stop("The table ", to, " exists in the working directory. Use overwrite = TRUE to overwrite it")
  }

  if(!is.null(fun)) {
    from <- fun(from)
  }

  RSQLite::dbWriteTable(self$get_where()$data, to, from,
                        overwrite = overwrite,
                        append = append,
                        row.names = TRUE,
                        ...)

  private$set_metadata()
  if(index_row_names) {
    self$create_index(to, column = "row_names", index_name = paste0(substitute(to), "_", "row_names"))
  }
  invisible(self)
}
