#' dbR6_set_metadata
#'@keywords internal

dbR6_set_metadata <- function() {
  this_data <- self$get_where()$data
  path <- private$path
  filename <- private$filename

  # if(filename != ":memory:") {
  #    path[[1]] <- paste0(getwd(), "/", filename, ".sqlite")
  #    path[[2]] <-  paste0(getwd(), "/", filename, ".json.gz")
  # } else {
  #    path[[1]] <- ":memory:"
  #    path[[2]] <- ":memory:"
  # }


  in_memory <- this_data@dbname == ":memory:"
  df_names <- self$list_tables()
  if(!in_memory) {
    db_size <- as.numeric(na.omit(file.size(self$get_where()$data@dbname)))
  } else {
    db_size <- 0
  }

  Robject_size <- as.numeric(pryr::object_size(self))

  # in db
  if(!in_memory) {
    con <- gzfile(path[[2]], "w")
    write(jsonlite::toJSON(list(df_names = df_names, db_size = db_size,
                                Robject_size = Robject_size, keys = private$keys,
                                nrows_tables = private$nrows_tables)), con)
    close(con)
  }

  # create an in-memory copy of metadata
  private$set_one_metadata_value(df_names, df_names)
  private$set_one_metadata_value(db_size, db_size)
  private$set_one_metadata_value(Robject_size, Robject_size)
  # private$set_one_metadata_value(path, path)
  invisible(NULL)
}
