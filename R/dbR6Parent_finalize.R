#' dbR6Parent_finalize__
#'@keywords internal

dbR6Parent_finalize <- function() {
    with(parent.env(environment()), {
    RSQLite::dbDisconnect(private$where$data)
    print("dbR6 connection closed\n")
    })
}

