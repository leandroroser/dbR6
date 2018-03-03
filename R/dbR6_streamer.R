#' dbR6_streamer
#' @example
#' {
#' data(mtcars2)
#' # the function requires an ID column with the number of the rows
#' mtcars <- data.frame(id=1:nrow(mtcars), mtcars)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' fun <- function(x) { x$carb <- x$carb + 10; x }
#' my_db$streamer("mtcars_db", to="out", fun = fun, n = 5)
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
    if(x == 1)  self$copy_table_structure(from = from, to = to, overwrite =  TRUE)
    this_query <- self$send_query(paste0("SELECT * FROM ", from,
                                         " WHERE id IN (SELECT id FROM ",
                                         from, " WHERE ", "id >= ", x,
                                         " AND id < ", x + n, ")"))

    ## move to 'to' and remove in each cycle

    self$add_table(from = this_query, to = to, fun = fun, append = TRUE, index_row_names = FALSE)

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

    if(index_row_names) {
       self$create_index(to, column = "row_names",
       index_name = paste0(substitute(to), "_", "row_names"))
     }
  })
  cat("Process finished in ", this_time[3], " seconds")
  invisible(self)
}
