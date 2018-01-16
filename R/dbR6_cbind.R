#' dbR6_cbind__
#'@keywords internal

dbR6_cbind <- function(...) {
  if(grep(outname, data_on_disk$list_tables())) {
    if(!overwrite) {
      stop("the table ", outname, " exists, but the parameter overwrite = FALSE")
    } else {
      data_on_disk$remove_table(outname)
    }
  }


  join <- match.arg(join)
  table_list <- list(...)
  joinlist <- list()

  if(join == "left" || join == "inner") {

    if(join == "left") {
      jointype <- "INNER JOIN"
    } else {
      jointype <- "LEFT OUTER JOIN"
    }

    alias_names <- as.character(outer(LETTERS, LETTERS, "paste0"))
    for(i in seq_along(table_list)) {
      joinlist[[i]] <- paste0(jointype," ",  table_list[[i]]," ", alias_names[i+1],  " ON ", alias_names[1], ".", using_what, " = ", alias_names[i+1], ".", using_what, " ")
    }
    joinlist[[1]] <- paste0("CREATE TABLE ", outname, " AS SELECT * FROM ",t1," ", alias_names[1], " ", joinlist[[1]])
    my_query <- do.call("paste0", joinlist)
  } else if (join == "cross") {
    for(i in seq_along(table_list)) {
      joinlist[[j]] <- paste0("CROSS JOIN ", table_list[[i]], " ")
    }
  } else {
    for(i in seq_along(table_list)) {
      joinlist[[j]] <- paste0("NATURAL JOIN ", table_list[[i]],  " USING ", using_what)
    }
    joinlist[[1]] <- paste0("CREATE TABLE ", outname, " AS SELECT * FROM ", t1, " ", joinlist[[1]])
    my_query <- do.call("paste", joinlist)
  }

  self$send_statement(my_query)

  invisible(self)
}
