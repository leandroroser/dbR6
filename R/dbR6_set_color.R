#' set_color
#' @name set_color
#' @example
#' {
#' data(mtcars2)
#' my_db <- dbR6$new()
#' my_db$add_table(mtcars, "mtcars_db")
#' }
#' @rdname set_color
#' @aliases set_color,dbR6
#' @exportMethod set_color


dbR6_set_color  <- function(...)  {
    bg_color <- match.arg(bg_color)
    if(bg_color == "auto") {
      # detect EStudio session color (if using RStudio)
      this_col <- try(rstudioapi::getThemeInfo(), silent = TRUE)
      if(class(this_col) == "try-error") {
        col_bg <- NULL
      } else {
        col_bg <- try(rstudioapi::getThemeInfo()$dark, silent = TRUE)
        # patch for Dracula
        col_bg2 <- try(rstudioapi::getThemeInfo()$editor == "Dracula",
          silent = TRUE)
        col_bg <- col_bg || col_bg2
      }
    } else {
      col_bg <- ifelse(bg_color == "white", FALSE, TRUE)
    }

    ### colors if object in R session###

    if(!is.null(col_bg)) {
      if(col_bg == "TRUE") {
        palette <- function(before, after, space) {
          bgCol <<- crayon::make_style("skyblue4", bg = TRUE)
          topCol <<-  bgCol$black
          add_space_color(before, after, space,
            crayon::bgCyan$black,
            crayon::bgCyan,
            crayon::bgMagenta$black)
        }

      } else {
        palette <- function(before, after, space) {
          bgCol <<- crayon::make_style("skyblue4", bg = TRUE)
          topCol <<- bgCol$white
          add_space_color(before, after, space,
            crayon::bgCyan$white,
            crayon::bgCyan,
            crayon::bgMagenta$white)
        }
      }
      attr(palette, "color") <-  "color"
    }

    if(is.null(col_bg) || bg_color == "none") {
      palette <- add_space
      attr(palette, "color") <-  "bw"
    }

    private$palette <- palette
    invisible(self)
}
