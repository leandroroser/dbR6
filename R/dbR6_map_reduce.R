
#' dbR6_map_reduce
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new("test", overwrite = TRUE)
#' my_db$add_table(mtcars, "mtcars_db")
#' fun <-  function(x) paste0("SELECT * FROM ", x, " WHERE ", paste0(x, ".mpg"), " > 20")
#' my_db$map_reduce(from = "mtcars_db", to = "map_reduce_result", column = "carb", query_function = fun)
#' my_db$list_tables()
#' my_db$get_table("map_reduce_result")
#' }
#' @keywords internal

dbR6_map_reduce <- function(...) {

  if(is.null(to)) to <- paste0(from, "_", "map_reduced")

  location <- self$get_where()$data@dbname
  # create output
  self$copy_table_structure(from, to, overwrite = overwrite)

  if(index_column) {
    index_name <- paste0(from, "_", column)
    if(!(self$list_indices() %in% my_db$list_indices()[,1])) {
     cat("Note: Index already exists. Overwriting...\n")
     self$drop_index(index_name)
    }
    self$create_index(tabname = from, column = column, index_name = index_name)
  }

  if(run_parallel) {
    if(location == ":memory:") {
      stop("Parallelization only available for on-disk files")
    }
    detect_workers <- parallel::detectCores(logical =  ifelse(physical, FALSE, TRUE))
    if(is.null(workers)) {
      workers <- detect_workers - 1
    } else {
      if(workers > detect_workers) {
        stop("The maximum number of workers available is: ", detect_workers, ".
             It is recommended to use ", detect_workers - 1, "cores")
      }
      }
    if(workers != 1)  {
      # if no cl_type specified, use PSOCK for windows and FORK otherwise
      if(is.null(cl_type)) {
        this_os <- get_os()
        if(this_os == "windows") {
          cl_type <- "PSOCK"
        } else {
          cl_type <- "FORK"
        }
      }

      } else {
        run_parallel <- FALSE
      }
    }

  my_factor <- self$send_query(paste0("SELECT DISTINCT ",column, " FROM ", from))[, 1]
  my_factor <- sort(my_factor)


  if(run_parallel) {


    cat("Configuring cluster...\n")
    cl <- parallel::makeCluster(workers, outfile = "", type = cl_type)

    on.exit((function(){
      cat("Stopping cluster...\n")
      parallel::stopCluster(cl)
      cat("done!\n")
    })())

    parallel::clusterExport(cl, varlist=c("location", "timeout", "query_function",
                                           "my_factor", "from", "column"),
                            envir = environment())
    parallel::clusterEvalQ(cl, {
      library(dbR6)
      #con$send_query("PRAGMA journal_mode = WAL;")
      NULL
    })

   cat("Running functions with ", workers, " workers...\n")
   parallel::parLapply(cl, seq_along(my_factor), function(i) {
      con <- dbR6$new(location)
      on.exit(con$finalize())
      con$send_query(paste0("PRAGMA busy_timeout=", timeout))
      temp_table <- paste0("__my_temp_table__", i)
      con$send_statement(paste0("DROP TABLE IF EXISTS ", temp_table))
      con$send_statement(paste0("CREATE TABLE ", temp_table, " AS SELECT * FROM ",
                                from, " WHERE ", paste0(from, ".", column),
                                " = ","'", my_factor[i], "'"))
      con$send_statement(paste0("INSERT INTO ", to, " ", query_function(temp_table)))
      con$send_statement(paste0("DROP TABLE ", temp_table))
      NULL
    })

  } else {
    for(i in seq_along(my_factor)) {
      temp_table <- paste0("__my_temp_table__", i)
      self$send_transaction(list(paste0("DROP TABLE IF EXISTS ", temp_table),
                                 paste0("CREATE TABLE ", temp_table, " AS SELECT * FROM ",
                                        from, " WHERE ", paste0(from, ".", column),
                                        " = ","'", my_factor[i], "'"),
                                 paste0("INSERT INTO ", to, " ", query_function(temp_table)),
                                 paste0("DROP TABLE ", temp_table)
                                 )
                            )
    }
    cat("done!\n")
  }

  private$set_metadata()
  invisible(self)
}
