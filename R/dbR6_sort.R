#' dbR6_sort__
#'@keywords internal

dbR6_sort  <- function(...)   {
  with(parent.env(environment()), {
  dots <- as.character(unlist(list(...)))
  if(length(dots) > 0) column <- paste(c(column, dots), collapse = ", ")

  # > cost of time (create index before?)
  #self$send_statement(paste0("CREATE INDEX idx_temp ON ", what, " (", column, ")"))

  tempname <- paste("temp_", paste(sample(c(letters, 0:9, 20)), collapse = ""), sep = "")
  self$copy_table_structure(tempname, what, overwrite = TRUE)
  self$send_statement(paste0("INSERT INTO ", tempname, " SELECT * FROM ", what, " ORDER BY ", column))
  self$remove_table(what)
  self$send_statement(paste0("ALTER TABLE ", tempname, " RENAME TO ", what))

  #self$send_statement("DROP INDEX IF EXISTS idx_temp") # but is previously removed when drop

  invisible(self)
  })
}
