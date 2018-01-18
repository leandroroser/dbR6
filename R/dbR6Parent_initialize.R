#' dbR6Parent_initialize__
#'@keywords internal

dbR6Parent_initialize <- function(...)  {
  # if file is xxx.sqlite, remove the .sqlite part
  if(length(grep(".sqlite$", filename, ignore.case = TRUE)) != 0) {
    filename <- gsub(".sqlite$", "", filename, ignore.case = TRUE)
  }

  # return_value = TRUE if an existing database is read from disk.
  # This is an indicator to detect if exists previous metadata
  return_value <- FALSE

  # automatically add file type
  if(filename != ":memory:") {
    data_name <- paste0(filename, ".sqlite")
    metadata_name <-  paste0(filename, ".json.gz")
  } else {
    data_name <- ":memory:"
    metadata_name <- ":memory:"
  }

  if(overwrite){
    if(filename == ":memory:") {
      stop("A file must be selected when overwrite is TRUE\n")
    }

    if(length(grep(data_name, dir())) != 0) {
      suppressMessages(file.remove(data_name))
      message("Overwriting database...")
    } else {
      message("Creating new database...\n")
    }
    return_value <- FALSE
    file.create(data_name)
    file.create(metadata_name)

  } else {
    if(filename != ":memory:") {
      if(length(grep(data_name, dir())) != 0) {
        message(paste0("Connecting with existing database: ", data_name, "\n"))
        if(!file.exists( metadata_name )) {
          message("No metadata found for sqlite object. Creating new metadata...\n")
          file.create(metadata_name)
        }
        return_value <- TRUE
      } else {
        message("Creating new database...\n")
        file.create(data_name)
        file.create(metadata_name)
        return_value <- FALSE
      }
    }
  }
  if(data_name != ":memory:") {
    data_name <- normalizePath(data_name)
    metadata_name <- normalizePath(metadata_name)
  }

  private$where <- new.env(parent = emptyenv(), hash = FALSE)
  private$where$data <- RSQLite::dbConnect(RSQLite::SQLite(), data_name)
  private$where$metadata <- metadata_name

  if(data_name == ":memory:") {
    message("Database created in memory\n")
  } else {
    message(paste0("Database located in: ", data_name))
  }

  return_value
}

