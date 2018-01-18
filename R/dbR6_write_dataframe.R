#' dbR6_write_dataframe__
#'@keywords internal

dbR6_write_dataframe <- function(...) {

    my_reader <- chunkR::reader(path = input, sep = sep, has_rownames = has_colnames,
                                has_colnames = has_colnames, chunksize =  chunksize,
                                data_format = "data.frame", columns_classes = columns_classes,
                                autodetect = autodetect, scan_rows = scan_rows)

    lines_written <- 0

    while(chunkR::next_chunk(my_reader)) {
      data <- chunkR::get_table(my_reader)

      if(lines_written == 0) {
        self$add_table(output, data, overwrite = TRUE, fun = fun, ...)
      } else {
        self$add_table(output, data, append = TRUE, fun = fun, ...)
      }

      lines_written <- lines_written +  nrow(data)
      cat("Written ", lines_written, " lines into database \n")
    }

    invisible(NULL)
}

# Old version based in read.table

# dbR6_write_dataframe <- function(...) {
#   with(parent.env(environment()), {
#   lines_completed <- 0
#
#   con <- file(description=input, open = "r")
#
#
#   if(has_rownames) {
#     rownames_value = 1
#   } else {
#     rownames_value <- NULL
#   }
#
#
#   if(has_colnames)
#   {
#     true_header <- read.table(con,
#                               nrows = 1,
#                               header = FALSE,
#                               sep = sep,
#                               na.strings = "NA",
#                               strip.white = TRUE,
#                               comment.char="",
#                               stringsAsFactors = FALSE,
#                               row.names = NULL,
#                               ...)
#     true_header<- as.character(true_header[1, ])
#   }
#
#   data <- read.table(con,
#                      header = FALSE,
#                      sep = sep,
#                      na.strings = "NA",
#                      strip.white = TRUE,
#                      comment.char="",
#                      stringsAsFactors = FALSE,
#                      nrows = chunksize,
#                      row.names = rownames_value,
#                      ...)
#
#   if(has_colnames) {
#     colnames(data) <- true_header
#   }
#
#   self$add_table(output, data, overwrite = TRUE, write_rownames = has_rownames)
#   lines_completed <- this_lines <- nrow(data)
#
#   while(this_lines > 0) {
#
#     tryCatch({
#       data <- read.table(con,
#                          header = FALSE,
#                          sep = sep,
#                          na.strings = "NA",
#                          strip.white = TRUE,
#                          comment.char="",
#                          stringsAsFactors = FALSE,
#                          nrows = chunksize,
#                          row.names = rownames_value,
#                          ... )
#       this_lines <- nrow(data)
#       lines_completed <- lines_completed  +  this_lines
#       if(this_lines > 0) {
#         if(has_colnames) {
#           colnames(data) <- true_header
#         }
#
#         if(!is.null(fun)){
#           data <- fun(data)
#         }
#
#         self$add_table(output, data, append = TRUE )
#         cat("Written ", lines_completed, " lines into database\n")
#       }
#     },
#     error = function(e) {
#       data <<-data.frame()
#       this_lines <<- 0
#       # only pass when all the lines were read
#       if(length(grep("no lines available in input", e$message)) == 0) {
#         stop(e)
#       }
#     })
#   }
#   close(con)
#   invisible(NULL)
#   })
# }
#
