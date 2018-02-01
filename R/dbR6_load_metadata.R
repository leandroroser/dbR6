#' dbR6_load_metadata
#' Internal method to load metadata
#' @keywords internal

dbR6_load_metadata  <- function() {
  # internal checkpoint for coding errors
  if(super$get_where()$data@dbname == ":memory:") stop("in memory file has not on-disk metadata")

  metadata_path <- self$get_where()$metadata
  #metadata is a json gz compressed
  con <- file(metadata_path, "rb")
  this_metadata <- suppressWarnings(try(jsonlite::fromJSON(gzcon(file(metadata_path, "rb"))), silent = TRUE))
  if(class(this_metadata) == "try-error") {
    private$set_metadata()
    this_metadata <- jsonlite::fromJSON(gzcon(file(metadata_path, "rb")))
  }
  private$set_one_metadata_value(df_names, this_metadata$df_names)
  private$set_one_metadata_value(db_size, this_metadata$db_size)
  private$set_one_metadata_value(Robject_size, this_metadata$Robject_size)
  private$keys <- this_metadata$keys
  close(con)
  invisible(NULL)
}

