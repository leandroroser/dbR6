
add_space <- function(before, after, char_number) {
  word_len <- nchar(before) + nchar(after)
  howmuch <-  char_number - word_len
  if(howmuch < 0) {
    stop(paste0("words length > space to fit (min: ", word_len, ")"))
  }
  cat(before, paste(rep(" ", howmuch), collapse = ""), after)
}

add_space_color <- function(before, after, char_number, bc, sc, ac) {
  word_len <- nchar(before) + nchar(after)
  howmuch <-  char_number - word_len
  if(howmuch < 0) {
    stop(paste0("words length > space to fit (min: ", word_len, ")"))
  }
  cat(bc(before), sc(paste(rep(" ", howmuch), collapse = "")), ac(after), sep = "")
}

#' generate a formatted number with corresponding units for plyr object
aux_format_object_size <- function(x) {
  units <- list(B = 1, KB = 1024, MB = 1024^2, GB = 1024^4, TB = 1024^4, PB = 1024^5, EB = 1024^6)
  temp <- x
  power_x <- 1

  while(temp / 1000 > 1) {
    power_x <- power_x + 1
    temp <- temp / 1000
  }
  #list(value = round(x /units[power_x], 3),  units = names(units)[power_x])
  paste0(round(x /units[[power_x]], 3),  " ", units = names(units)[power_x])
}


#'call_dbR6
#'@keywords internal

call_dbR6 <- function(method, args) {
   if(missing(args)) {
     args <- list()
  }
  formals(method) <- args
  environment(method) <- parent.env(environment())
  method
}
