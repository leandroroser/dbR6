#' dbR6_streamer
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#'
#' # COMPLETAR---
#' }
#' @keywords internal


dbR6_streamer  <- function(...) {
  ## it is assuming an id in the table, may be changed

  force(x <- 1)
  force(n)
  force(to)

  # iterator
  iter_fun <- function() {
    if(x == 1)  self$copy_table_structure(to, from = from,  overwrite =  TRUE)
    this_query <- self$send_query(paste0("SELECT * FROM ", from,
                                         " WHERE id IN (SELECT id FROM ", from, " WHERE ", "id >= ", x,
                                         " AND id < ", x + n, ")"))

    ## move to to and remove in each cycle

    self$add_table(new_name = to, new_df = my_fun(this_query), append = TRUE)

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
}
