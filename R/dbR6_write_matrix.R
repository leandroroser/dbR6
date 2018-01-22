#' dbR6_write_matrix__
#'@keywords internal

dbR6_write_matrix <- function(...) {

    my_reader <- chunkR::reader(path = from, sep = sep, has_rownames = has_colnames,
                                has_colnames = has_colnames, chunksize =  chunksize,
                                data_format = "matrix")

  lines_written <- 0

  while(chunkR::next_chunk(my_reader)) {
    data <- chunkR::get_table(my_reader)

    if(data_mod != "character") {
      mode(data) <- data_mod
    }

    if(!is.null(fun)){
      data <- fun(data)
    }

    data <- chunkR::matrix2df(data)
    if(lines_written == 0) {
      self$add_table(to, data, index_row_names = FALSE,
                     overwrite = overwrite, ...)
    } else {
      self$add_table(to, data, append = TRUE, index_row_names = FALSE, ...)
    }
    lines_written <- lines_written +  nrow(data)
    cat("Written ", lines_written, " lines into database \n")
  }

  if(index_row_names) {
    self$create_index(to, column = "row_names")
  }

  invisible(NULL)
}

