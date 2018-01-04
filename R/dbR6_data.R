

#' dbR6_data class
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @field where environment storing data & enclosing environment for metadata
#' @section Methods:
#' \describe{
#' \item{initialize}{initialize method}
#' \item{finalize}{finalize method}
#' \item{get_where}{get environment with the database connection}
#' \item{set_data}{set database connection}
#' }
#' @export


dbR6_data <- R6::R6Class("dbR6_data",

private = list(

where = NULL

),

public = list(

initialize = function(filename = ":memory:", overwrite = FALSE) {


    if(overwrite){
      if(filename == ":memory:") {
        stop("A file must be selected when overwrite is TRUE")
      }

      if(length(grep(filename, dir())) != 0) {
        suppressMessages(file.remove(filename))
        message("Overwriting database...")
      } else {
        message("Creating new database...")
      }
      file.create(filename)

    } else {
      if(filename != ":memory:") {
        if(length(grep(filename, dir())) != 0) {
        message(paste0("Connecting with existing database: ", filename))
      } else {
        message("Creating new database...")
        file.create(filename)
      }
      }
    }
    if(filename != ":memory:") filename <- normalizePath(filename)
    private$where <- new.env(parent = emptyenv(), hash = FALSE)
    private$where$data <- RSQLite::dbConnect(RSQLite::SQLite(), filename)

    if(filename == ":memory:") {
      message("Database created in memory")
    } else {
      message(paste0("Database located in: ", filename))
    }
  },

  #------------------
finalize = function() {
    RSQLite::dbDisconnect(private$where$data)
    print("dbR6 connection closed")
  },

  #------------------
get_where = function() private$where,

  #-----------------
set_data = function(x) private$where$data <- x
)
)
