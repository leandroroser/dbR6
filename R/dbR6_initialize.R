#' initialize
#' @name initialize
#'@keywords internal

dbR6_initialize <- function(...) {
  bg_color <- match.arg(bg_color)
  exists_metadata <- super$initialize(filename, overwrite)
  private$metadata <- new.env(parent = self$get_where(), hash = FALSE)

  if(exists_metadata) {
    metadata_path <- paste0(private$path[[2]])

    if(!file.exists(metadata_path) && !new_metadata) {
      stop("No metadata found for object. New metadata without keys can be created using the parameter
                new_metadata = TRUE. Note that a splitted table can not be reduced with the reduce() method
                when the original metadata has been lost and you should do a manual reduction via rbind()
                (see the rbind() method documentation.")
    } else if(!file.exists(metadata_path) && new_metadata) {
      private$set_metadata()
    } else {
      private$load_metadata()
    }

  } else {
    private$set_metadata()
  }

  self$set_color(bg_color = bg_color)
  invisible(self)
}

