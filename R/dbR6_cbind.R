#' dbR6_cbind
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$cbind("out", using_what = "row_names", "mtcarsdb", "mtcarsdb")
#' }
#' @keywords internal

dbR6_cbind <- function(...) {

  join <- match.arg(join)

  # Convert tables names w/o quotes into strings & remove extra quotes added in strings by substitute
  #  to <- gsub('^"|"$', "", deparse(substitute(to)))
  if(to %in% self$list_tables()) {
    if(!overwrite) {
      stop("the table ", to, " exists, but the parameter overwrite = FALSE")
    } else {
      self$remove_table(to)
    }
  }

  # using_what <- gsub('^"|"$', "", deparse(substitute(using_what)))
  # table_list <- sapply(substitute(list(...))[-1], function(y) gsub('^"|"$', "", deparse(y)))
  table_list <- list(...)
  t1 <- table_list[1]
  table_list <- table_list[-1]

  joinlist <- list()

  if (join == "cross") {
    if(using_what == "row_names") {
      for(i in seq_along(table_list)) {
        joinlist[[i]] <- paste0("CROSS JOIN ", table_list[[i]], " ", "using(", what, ")")
      }
    } else {
      for(i in seq_along(table_list)) {
        joinlist[[i]] <- paste0("CROSS JOIN ", table_list[[i]], " ")
      }
    }

  } else {

    if(join == "inner") {
      jointype <- "INNER JOIN"
    } else if(join == "left") {
      jointype <- "LEFT OUTER JOIN"
    } else if(join == "natural") {
      jointype <- "NATURAL JOIN"
    }

    alias_names <- as.character(outer(LETTERS, LETTERS, "paste0"))

    if(join == "natural") {
      for(i in seq_along(table_list)) {
        joinlist[[i]] <- paste0(jointype," ",  table_list[[i]]," ","AS ", alias_names[i+1])
      }

    } else {
      for(i in seq_along(table_list)) {
        # joinlist[[i]] <- paste0(jointype," ",  table_list[[i]]," ","AS ", alias_names[i+1],
        #                         " ON ", alias_names[1], ".", using_what, " = ", alias_names[i+1],
        #                         ".", using_what)
        joinlist[[i]] <- paste0(jointype," ",  table_list[[i]]," ","AS ", alias_names[i+1],
                                " USING(", using_what, ")")
      }
    }
  }

  joinlist[[1]] <- paste0("CREATE TABLE ", to, " AS SELECT * FROM ",t1," ", "AS ", alias_names[1], " ", joinlist[[1]])
  my_query <- do.call("paste0", joinlist)
  self$send_statement(my_query)

  invisible(self)
}
