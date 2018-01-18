#' dbR6_filter__
#'@keywords internal

dbR6_filter  <- function(...)  {
  with(parent.env(environment()), {
  # R commands to be evaluated in the condition ('where' query)
  # are indicated witihin %rs& and %re% as in:  %rs% my_command %re%

  if(r_commands) {
    my_pattern <- gregexpr("(?<=%rs%).*?(?=%re%)", conditions, perl = TRUE)
    get_pattern <- regmatches(conditions, my_pattern)[[1]]
    input<-list()
    input[[1]] <-  unlist(lapply(get_pattern, function(x) eval(parse(text = x))))
    regmatches(conditions, my_pattern) <- input
    conditions <- gsub("%rs%|%re%", "", conditions)
  }

  conditions <- gsub("[|]+|[||]+", "OR", conditions)
  conditions <- gsub("[&]+|[&&]+", "AND", conditions)
  self$send_query(paste0("SELECT * FROM ", table, " WHERE ", conditions))
})
}
