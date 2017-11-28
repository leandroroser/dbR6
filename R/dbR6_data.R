

#' dbR6_data class
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @field where environment storing data & enclosing environment for metadata
#' @section Methods:
#' \describe{
#' \item{initialize}{ipsus lorum}
#' \item{finalize}{ipsus lorum}
#' \item{get_where}{ipsus lorum}
#' \item{set_data}{ipsus lorum}
#' }
#' @export


dbR6_data <- R6::R6Class("dbR6_data",

private = list(

where = NULL

),

public = list(

initialize = function(filename = ":memory:", overwrite = FALSE) {

    if(filename == ":memory:" && overwrite) {
      stop("A file must be selected when overwrite is TRUE")
    }

    if(overwrite){
      if(length(grep(filename, dir())) != 0) {
        suppressMessages(file.remove(filename))
        message("Overwriting database...")
      }
    }

    if(filename != ":memory:" && !overwrite) {
      message(paste0("Connecting with database: ", filename))
    }

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
