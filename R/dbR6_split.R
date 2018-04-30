#' dbR6_split
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

dbR6_split <- function(...) {
    my_factor <- self$send_query(paste0("SELECT DISTINCT ",column, " FROM ", from))[, 1]
    my_factor <- sort(my_factor)

    statement_fun <- function(y) paste0("CREATE TABLE ", y, " AS SELECT * FROM ",
                                        from, " WHERE ", paste0(from, ".", column),
                                        " = ","'", my_factor[i], "'")
    this_table <- paste0(column, "_", my_factor)

    if(any(this_table %in% self$list_tables())) {
      if(!overwrite) {
        stop("some of output tables exist, but the parameter overwrite = FALSE")
      } else {
        self$remove_table(this_table)
      }
    }

    for(i in seq_along(my_factor)) {
      my_statement <- statement_fun(this_table[i])
      self$send_statement(my_statement)
    }
    # remove parent table
    if(remove_after) {
      self$remove(column)
    }
    private$add_keys(column, my_factor)
    private$set_metadata()
    invisible(self)
  }

