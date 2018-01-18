
#' dbR6_set_metadata__
#'@keywords internal

dbR6_set_metadata <- function() {
  this_data <- super$get_where()$data
  metadata_path <- self$get_where()$metadata
  in_memory <- this_data@dbname == ":memory:"
  df_names <- self$list_tables()
  if(!in_memory) {
    db_size <- round(as.numeric(na.omit(file.size(super$get_where()$data@dbname))) / 1E3, 3)
  } else {
    db_size <- 0
  }

  Robject_size <-  round(as.numeric(pryr::object_size(self))/ 1E6, 3)

  # in db
  if(!in_memory) {
    con <- gzfile(metadata_path, "w")
    write(jsonlite::toJSON(list(df_names = df_names, db_size = db_size,
                                Robject_size = Robject_size, keys = private$keys)),con)
    close(con)
  }

  # create an in-memory copy of metadata
  self$set_one_metadata_value(df_names, df_names)
  self$set_one_metadata_value(db_size, db_size)
  self$set_one_metadata_value(Robject_size, Robject_size)
  invisible(NULL)
}
