#' dbR6_write_matrix__
#'@keywords internal

dbR6_write_matrix <- function(...) {

    my_reader <- chunkR::reader(path = input, sep = sep, has_rownames = has_colnames,
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
      self$add_table(output, data, overwrite = TRUE, ...)
    } else {
      self$add_table(output, data, append = TRUE, ...)
    }
    lines_written <- lines_written +  nrow(data)
    cat("Written ", lines_written, " lines into database \n")
  }

  invisible(NULL)
}

