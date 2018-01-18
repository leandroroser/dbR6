
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
