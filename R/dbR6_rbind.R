#' dbR6_location
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars[1:10, ], "mtcars_db1")
#'my_db$add_table(mtcars[11:20, ], "mtcars_db2")
#' my_db$rbind("mtcars_db1", "mtcars_db2")
#'my_db$rbind("out","mtcars_db1", "mtcars_db2")
#'
#'
#' my_db2 <- dbR6$new()
#' my_db2$add_table(mtcars[1:10, ], "mtcars_db1")
#' my_db2$add_table(mtcars[11:20, ], "mtcars_db2")
#' my_db2$rbind("mtcars_db1", "mtcars_db2")
#' my_db2$rbind("out", "mtcars_db1", "mtcars_db2", remove_appended = "sequential")
#'
#' my_db3 <- dbR6$new()
#' my_db3$add_table(mtcars[1:10, ], "mtcars_db1")
#' my_db3$add_table(mtcars[11:20, ], "mtcars_db2")
#' my_db3$rbind("mtcars_db1", "mtcars_db2")
#' my_db3$rbind("out", "mtcars_db1", "mtcars_db2", remove_appended = "after")
#'
#'
#' }
#' @keywords internal


dbR6_rbind  <- function(...) {

  remove_appended <- match.arg(remove_appended)

  if(length(grep(to, self$list_tables())) > 0) {
    if(!overwrite) {
      stop("the table ", to, " exists, but the parameter overwrite = FALSE")
    } else {
      self$remove_table(to)
    }
  }

  table_list <- unlist(list(...))
  self$copy_table_structure(table_list[1], to, overwrite = overwrite)

  for(table_to_append in table_list) {
    self$send_statement(paste0("INSERT INTO ", to, " SELECT * FROM ", table_to_append))

    if(remove_appended == "sequential") {
      self$remove_table(table_to_append)
    }
  }

  if(remove_appended == "after") {
    for(table_to_append in table_list) {
      self$remove_table(table_to_append)
    }
  }

  invisible(self)
}
