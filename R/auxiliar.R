
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


#'generate_method
#'@keywords internal

# generate_method <- function(method, dots = FALSE, ...) {
#   #method <- eval(method)
#   #my_formals <- match.call(expand.dots = FALSE)$`...`
#   my_formals <- as.list(...)
#
#   # add ellipsis if required
#   if(dots) {
#     temp <- alist(...=)
#     for(i in seq_along(my_formals)) {
#       eval(parse(text = paste0("temp$", names(my_formals)[i], " <- ", my_formals[i])))
#     }
#     my_formals <- temp
#   }
#
#   formals(method) <- my_formals
#   environment(method) <- parent.env(parent.env(environment()))
#   method
# }

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


#' Generate template for dbR6 method
#' @keywords internal

generate_template <- function(external_fun, args = TRUE, what_class) {

  external_fun <- paste0(what_class, "_", external_fun)
  outfile <- paste0(external_fun, ".R")
  file.create(outfile)

  internal_fun <- paste0(external_fun, "__")

  sink(outfile)
  cat("#'", internal_fun, "\n")
  cat("#'@keywords internal\n\n")

  cat(internal_fun, paste0(" <- function(", ifelse(args, "...", ""), ")"), "{\n\n}\n\n")

  cat("#'", external_fun, "\n")
  cat("#'@keywords internal\n\n")

  cat(external_fun,
      paste0("<- function(", ifelse(args, "...", ""), ")"),
      "{\n",  "generate_method(method = ", internal_fun, paste0(ifelse(args, ", ...", ""), ")"), "\n}\n")
  sink()

  cat("Template generated in ", outfile, "\n")
  invisible(NULL)
}
