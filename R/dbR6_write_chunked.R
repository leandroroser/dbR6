#' dbR6_write_chunked
#' @example
#' {
#' data(mtcars2)
#' write.table(mtcars, "mtcars.txt")
#' my_db <- dbR6$new()
#' my_db$write_chunked("mtcars.txt", to = "mtcars_db", chunksize = 10)
#' my_db$get_table("mtcars_db")
#' }
#' @keywords internal

dbR6_write_chunked <- function(...) {

    my_chunk <- chunkR::chunker(path = from, sep = sep, has_rownames = has_rownames,
                                has_colnames = has_colnames, chunksize =  chunksize,
                                data_format = "data.frame", columns_classes = columns_classes,
                                autodetect = autodetect, scan_rows = scan_rows)

    lines_written <- 0

    while(chunkR::next_chunk(my_chunk)) {
      data <- chunkR::get_table(my_chunk)

      if(lines_written == 0) {
        self$add_table(data, to, overwrite = overwrite,
                       fun = fun,
                       index_row_names = FALSE,
                       ...)
      } else {
        self$add_table(data, to, append = TRUE, fun = fun, index_row_names = FALSE, ...)
      }

      lines_written <- lines_written +  nrow(data)
      cat("Written ", lines_written, " lines into database \n")
    }

    if(index_row_names) {
      self$create_index(to, column = "row_names",
                        index_name = paste0(substitute(to), "_", "row_names"))
    }

    invisible(NULL)
}
