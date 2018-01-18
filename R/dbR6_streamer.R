#' dbR6_streamer__
#'@keywords internal

dbR6_streamer  <- function(...) {
  with(parent.env(environment()), {
  ## it is assuming an id in the table, may be changed

  force(x <- 1)
  force(n)
  force(output)

  # iterator
  iter_fun <- function() {
    if(x == 1)  self$copy_table_structure(output, from = input,  overwrite =  TRUE)
    this_query <- self$send_query(paste0("SELECT * FROM ", input,
                                         " WHERE id IN (SELECT id FROM ", input, " WHERE ", "id >= ", x,
                                         " AND id < ", x + n, ")"))

    ## move to output and remove in each cycle

    self$add_table(new_name = output, new_df = my_fun(this_query), append = TRUE)

    x_0 <- x
    nr <- nrow(this_query)
    x <<- x + n
    if(nr == 0 && x_0 != 1)
    {
      return(0)
    } else {
      return(x-1)
    }
  }

  j <- 1
  this_time <- system.time({
    while(j != 0) {
      j <- iter_fun()
      if(j != 0) cat(paste0("Yield ", j, "\n"))
    }
  })
  cat("Process finished in ", this_time[3], " seconds")
  invisible(self)
  })
}
