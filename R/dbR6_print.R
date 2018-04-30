#' dbR6_print__
#'@keywords internal

dbR6_print <- function() {
    if(!self$is_valid()) {
      message("Invalid dbR6 object\n")
      return(invisible(NULL))
    }

    palette <- private$palette
    tables <- self$list_tables()
    if(all(tables %in% "")) {
      print_tables <- " [[empty db]] "
    } else {
      print_tables <- paste0(" ", paste(tables, collapse = ", "), " ")
      if(nchar(print_tables) > 33) {
        print_tables <- paste0(" ",
          substr(print_tables, 1, 16), "... [",
          self$get_tables_number(), " table(s)] ")
    }
    }

    print_obj_size <- paste0(" ",
      aux_format_object_size(self$get_metadata()$Robject_size), " ")

    in_memory <- self$get_where()$data@dbname == ":memory:"
    if(in_memory) {
      print_db_size <- " [[in memory]] "
    } else {
      print_db_size <- paste0(" ",
        aux_format_object_size(self$get_metadata()$db_size), " ")
    }

    # simplify path if there only are > 2 levels
    print_location <-  self$location()
    if(gsub("(.*/)*(.*?/)(.*?$)", "\\1", print_location) != "") {
      print_location <- gsub("(.*/)*(.*?/)(.*?$)", ".../\\2\\3 ",
                        print_location)
    } else {
      print_location <- paste0(self$location(), " ")
    }

    if(nchar(print_location) > 33) {
      end <- nchar(print_location)
      start <- end - 30
      print_location <- paste0(" ...", substr(print_location, start, end))
    } else {
      print_location <- paste0(" ", print_location)
    }

    if(attr(palette, "color") == "color") {
      bgCol <- crayon::make_style("skyblue4", bg = TRUE)
      topCol <- bgCol$white
      arrow_left <- crayon::bgMagenta(" <-> ")
      top <- topCol("                    dbR6 object                           \n\n")
      bottom <- "                                                                      \n"
    } else {
      bgCol <- crayon::make_style("skyblue4", bg = TRUE)
      topCol <-  bgCol$black
      arrow_left <- " <-> "
      top <-    "------------------------ dbR6 object ------------------------\n"
      bottom <- "-------------------------------------------------------------\n"
    }
    cat("\n")
    cat(top)
    cat(arrow_left); palette(" Data frames: ", print_tables, 53); cat("\n")
    cat(arrow_left); palette(" Size of R object: ", print_obj_size, 53); cat("\n")
    cat(arrow_left); palette(" Size of db on disk: ", print_db_size, 53); cat("\n")
    cat(arrow_left); palette(" Location: ", print_location, 53); cat("\n")
    cat(bottom)
    invisible(self)
}
