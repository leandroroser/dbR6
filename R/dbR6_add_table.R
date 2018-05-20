#' add_table
#' @name add_table
#' @description Add a table to a dbR6  object
#' @param from Input table
#' @param to Name of the table in database
#' @param overwrite Overwrite table if already exists? Default FALSE
#' @param append  Append rows if the tablealready exists? Default FALSE
#' @param fun Function to apply to the table before writing it
#' @param index_row_names index row names? Default TRUE
#' @param ... Additional parameters passed to RSQLite::dbWriteTable
#' @usage my_object$add_table(from, to, overwrite, append fun, index_row_names)
#' @examples
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' }
#' @rdname add_table
#' @aliases add_table,dbR6
#' @exportMethod add_table

dbR6_add_table <- function(...) {

  if(to %in% self$list_tables() && !overwrite && !append) {
    stop("The table ", to,
         " exists in the working directory. Use overwrite = TRUE to overwrite it")
  }

  if(!is.null(fun)) {
    from <- fun(from)
  }

  if(length(grep("\\.", to))> 0) {
  message("Note: replacing . by _ in name\n")
  to <- gsub("\\.", "_", to)
  }

  if(class(from) == "data.frame") {
  RSQLite::dbWriteTable(self$get_where()$data, to, from,
                        overwrite = overwrite,
                        append = append,
                        row.names = row.names,
                        ...)

  } else if(class(from) == "tbl_dbi") {
    copy_to(self$get_connection(), df = from, name = to,
            overwrite = overwrite, temporary = FALSE, ...)
  }

  if(index_row_names && row.names) {
    self$create_index(to, column = "row_names",
                      index_name = paste0(substitute(to), "_", "row_names"),
                      overwrite = TRUE)
  }

  private$set_metadata()
  invisible(self)
}
