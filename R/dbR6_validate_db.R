#' dbR6_validate_db
#' Internal method
#' @keywords internal


dbR6_validate_db <- function() {
  private$valid <- RSQLite::dbIsValid(self$get_where()$data)
}
