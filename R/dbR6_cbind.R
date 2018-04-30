#' cbind
#' @name cbind
#' @param tables names of data frames included in a dbR6 data base
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$cbind("out", using_what = "row_names", "mtcarsdb", "mtcarsdb")
#' }
#' @rdname cbind
#' @aliases cbind,dbR6
#' @exportMethod cbind

dbR6_cbind <- function(...) {

  join <- match.arg(join)

  tempname <- paste(paste0(sample(LETTERS, 20), sample(seq_len(99), 10)), collapse = "")

  # Convert tables names w/o quotes into strings & remove extra
  # quotes added in strings by substitute
  #  to <- gsub('^"|"$', "", deparse(substitute(to)))
  if(to %in% self$list_tables()) {
    if(!overwrite) {
      stop("the table ", to,
        " exists, but the parameter overwrite = FALSE")
    } else {
      # if table not used for cbind, remove now. otherwise, remove after cbind
      if(!(to %in% tables)) {
      self$remove_table(to)
      }
    }
  }

  # using_what <- gsub('^"|"$', "", deparse(substitute(using_what)))
  # table_list <- sapply(substitute(list(...))[-1], function(y) gsub('^"|"$',
  # "", deparse(y)))
  # table_list <- list(...)

  t1 <- tables[1]
  table_list <- tables[-1]

  joinlist <- list()

  alias_names <- as.character(outer(LETTERS, LETTERS, "paste0"))


    if(join == "full") {

    full_join <- function(db, t1, t2, n1, n2, joincol, to) {
      if(is.null(n1)) {
      n1 <- paste(db$colnames(t1))
      n1 <- n1[-which(n1 == joincol)]
      }
      n1_alias <- paste(t1, ".", n1, collapse = ",", sep = "")
      n1_alias <- paste(joincol, n1_alias, sep = ",")
      if(is.null(n2)) {
      n2 <- db$colnames(t2)
      n2 <- n2[-which(n2 == joincol)]
      }
      n2_alias <- paste(t2, ".", n2, collapse = ",", sep = "")
      #n2_alias <-  paste(joincol, n2_alias, sep = ",")

      what <- paste(n1_alias, n2_alias, sep = ",")
      using_what <- paste0(" USING(", joincol, ")")

      where_statement <- paste0(" WHERE ", joincol,
                         " NOT IN (SELECT ", joincol, " from ", t2, ")")

      db$send_statement(paste0(" CREATE TABLE ", to, " AS SELECT ", what,
        " FROM ", t2," LEFT JOIN ", t1, using_what, " UNION ALL SELECT ", what,
        " FROM ", t1 , " LEFT JOIN ", t2, using_what, where_statement))


    }

    if(length(tables) != 2) {
      stop("the current full join implementation is for 2 tables")
    }
    full_join(self, tables[1], tables[2], vars_1, vars_2, using_what, tempname)

    } else {

      if (join == "cross") {
        if(using_what == "row_names") {
          for(i in seq_along(table_list)) {
            joinlist[[i]] <- paste0("CROSS JOIN ",
              table_list[[i]], " ", "using(", what, ")")
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

    if(join == "natural") {
      for(i in seq_along(table_list)) {
        joinlist[[i]] <- paste0(jointype," ",
          table_list[[i]]," ","AS ", alias_names[i+1])
      }

    } else {
      for(i in seq_along(table_list)) {
        joinlist[[i]] <- paste0(jointype," ",
          table_list[[i]]," ","AS ",
          alias_names[i+1],
          " USING(", using_what, ")")
      }
    }
    }

  # cbind mode---------

    joinlist[[1]] <- paste0("CREATE TABLE ",
      tempname, " AS SELECT * FROM ",t1," ", "AS ",
      alias_names[1], " ", joinlist[[1]])
    my_query <- do.call("paste0", joinlist)
    self$send_statement(my_query)

  }

    if(remove_after) {
      self$remove_table(tables)
    }

  # now remove table, this allows to overwrite the table if already exists
  if(to %in% self$list_tables()) {
    self$remove_table(to)
  }
  self$rename_table(tempname, to)

  if(index_row_names) {
    self$create_index(to, column = using_what,
      index_name = paste0(substitute(to), "_", using_what),
      overwrite = TRUE)
  }

  invisible(self)
}
