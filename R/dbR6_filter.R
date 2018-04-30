#' filter
#' @name filter
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' my_db$colnames("mtcars_db")
#' my_db$filter("mtcars_db", conditions = mpg>1)
#' my_db$filter("mtcars_db", conditions = paste0("mpg > 0 & cyl > ", mean(1:10)), eval_before = TRUE)
#' }
#' @rdname filter
#' @aliases filter,dbR6
#' @exportMethod filter


dbR6_filter  <- function(...)  {
  # R commands to be evaluated in the condition ('where' query)
  # are indicated witihin {{}} as in:  {{my_command}}
  if(!eval_before) {
  conditions<- gsub('^"|"$', "", deparse(substitute(conditions)))
  }

  # # find R patterns
  #   my_pattern <- gregexpr("(?<=//).*?(?=//)", conditions, perl = TRUE)
  #   #//{ match case
  #   if(my_pattern[[1]][1] != -1) {
  #   get_pattern <- regmatches(conditions, my_pattern)[[1]]
  #   input<-list()
  #   input[[1]] <-  unlist(lapply(get_pattern, function(x) eval(parse(text = x))))
  #   regmatches(conditions, my_pattern) <- input
  #   conditions <-gsub("//|//", "", conditions)
  #   }
    #//}

  conditions <- gsub("[|]+|[||]+", "OR", conditions)
  conditions <- gsub("[&]+|[&&]+", "AND", conditions)
  self$send_query(paste0("SELECT * FROM ", tabname, " WHERE ", conditions))
}
