#' dbR6_split__
#'@keywords internal

dbR6_split <- function(...) {
  with(parent.env(environment()), {
    my_factor <- self$send_query(paste0("SELECT DISTINCT ",what, " FROM ", x))[, 1]

    statement_fun <- function(y) paste0("CREATE TABLE ", y, " AS SELECT * FROM ",
                                        x, " WHERE ", paste0(x, ".", what),
                                        " = ","'", my_factor[i], "'")
    this_table <- paste0(what, "_", my_factor)

    if(any(grep(paste(this_table, collapse="|"), data_on_disk$list_tables()) == TRUE)) {
      if(!overwrite) {
        stop("some of output tables exist,", outname, " but the parameter overwrite = FALSE")
      } else {
        data_on_disk$remove_table(this_table)
      }
    }

    for(i in seq_along(my_factor)) {
      my_statement <- statement_fun(this_table[i])
      self$send_statement(my_statement)
    }
    # remove parent table
    if(remove_after) {
      self$remove(what)
    }
    self$add_keys(what, my_factor)
    self$set_metadata()
    invisible(self)
  })
  }
