
#' dbR6_list_tables__
#'@keywords internal

dbR6_list_tables = function() {
  out <- RSQLite::dbListTables(self$get_where()$data)
  out <- out[out != "metadata"]
  if(length(out) == 0)  {
    return("")
  } else return(sort(out))
}
