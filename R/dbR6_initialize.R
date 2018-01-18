#' dbR6_initialize
#'@keywords internal

dbR6_initialize <- function(...){
  with(parent.env(environment()), {
  exists_metadata <- super$initialize(filename, overwrite)
  private$metadata <- new.env(parent = super$get_where(), hash = FALSE)
  if(exists_metadata) {
    metadata_path <- paste0(super$get_where()$metadata)

    if(!file.exists(metadata_path) && !new_metadata) {
      stop("No metadata found for object. New metadata without keys can be created using the parameter
                new_metadata = TRUE. Note that a splitted table can not be reduced with the reduce() method
                when the original metadata has been lost and you should do a manual reduction via rbind()
                (see the rbind() method documentation.")
    } else if(!file.exists(metadata_path) && new_metadata) {
      self$set_metadata()
    } else {
      self$load_metadata()
    }

  } else {
    self$set_metadata()
  }
  })
}

