
#' dbR6 class
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom crayon bgCyan
#' @importFrom crayon bgMagenta
#' @importFrom crayon bold
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @field where (inherited from R6_data class) Environment storing data & enclosing environment for metadata
#' @field metadata Environment storing object metadata
#' @section Methods:
#' \describe{
#' \item{deep_clone}{ipsus lorum}
#' \item{initialize}{ipsus lorum}
#' \item{finalize}{ipsus lorum}
#' \item{get_where}{(inherited from R6_data class) ipsus lorum}
#' \item{set_data}{(inherited from R6_data class) ipsus lorum}
#' \item{get_metadata}{ipsus lorum}
#' \item{set_metadata}{ipsus lorum}
#' \item{set_metadata_single}{ipsus lorum}
#' \item{list_tables}{ipsus lorum}
#' \item{set_metadata}{ipsus lorum}
#' \item{colnames}{ipsus lorum}
#' \item{nrow}{ipsus lorum}
#' \item{ncol}{ipsus lorum}
#' \item{print}{ipsus lorum}
#' \item{get_table}{ipsus lorum}
#' \item{send_query}{ipsus lorum}
#' \item{send_statement}{ipsus lorum}
#' \item{add_table}{ipsus lorum}
#' \item{remove_table}{ipsus lorum}
#' \item{add_empty_table}{ipsus lorum}
#' \item{save}{ipsus lorum}
#' \item{clone_db}{ipsus lorum}
#' \item{streamer}{ipsus lorum}
#' }
#' @export

# retornando invisible(self) el objeto es encadenable

dbR6 <- R6::R6Class("dbR6",

inherit = dbR6_data,
private = list(
  metadata = NULL,
  deep_clone = function(name, value) {
    # With x$clone(deep=TRUE) is called, the deep_clone gets invoked once for
    # each field, with the name and value.
    head_env <- new.env(parent = emptyenv(), hash = FALSE)
    if (name == "where") {
      list2env(as.list.environment(value, all.names = TRUE), envir = head_env)
    } else if(name == "metadata") {
      list2env(as.list.environment(value, all.names = TRUE), parent = head_env, hash = FALSE)
    } else {
      # For all other fields, just return the value
      value
    }
  }
),

public = list(

  #---------------------
initialize = function(filename = ":memory:", overwrite = FALSE) {
    super$initialize(filename, overwrite)
    private$metadata <- new.env(parent = super$get_where(), hash = FALSE)
    self$set_metadata()
  },

  #----------------------
  finalize = function() {
    super$finalize()
  },

  #---------------------
get_metadata = function() private$metadata,

  #---------------------
set_metadata = function() {

    this_data <- super$get_where()$data
    in_memory <- this_data@dbname == ":memory:"
    df_names <- self$list_tables()
    if(!in_memory) {
      db_size <- round(as.numeric(na.omit(file.size(super$get_where()$data@dbname))) / 1000, 3)
    } else {
      db_size <- 0
    }

    Robject_size <-  round(as.numeric(pryr::object_size(self))/ 1000, 3)

    # in db
    if(!in_memory) {
      RSQLite::dbWriteTable(this_data, "metadata",
                            data.frame(df_names = df_names, db_size = db_size, Robject_size = Robject_size), overwrite = TRUE)
    }
    #in object

    self$set_metadata_single(df_names, df_names)
    self$set_metadata_single(db_size, round(as.numeric(db_size) / 1000, 3))
    self$set_metadata_single(Robject_size, round(as.numeric(Robject_size) / 1000, 3))
  },

  #--------------------
set_metadata_single = function(name, value) {
    name <- deparse(substitute(name))
    assign(name, value, private$metadata)
  },

  #----------------------
list_tables = function() {
    out <- RSQLite::dbListTables(super$get_where()$data)
    out <- out[out != "metadata"]
    if(length(out) == 0)  {
      return("")
    } else return(out)
  },

  #---------------------
colnames = function(what) {
    if(!what %in% self$list_tables()) return(paste0("Table '", what,   "' not found in database"))
    RSQLite::dbListFields(super$get_where()$data, what)
  },

  #--------------------
ncol = function(what) {
    if(!what %in% self$list_tables()) return(paste0("Table '", what,   "' not found in database"))
    out <- RSQLite::dbListFields(super$get_where()$data, what)
    length(out)
  },

  #---------------------
nrow = function(what) {
    if(!what %in% self$list_tables()) return(paste0("Table '", what,   "' not found in database"))
    out <- self$send_query(paste0("SELECT COUNT(*) FROM ", what))
    out[[1]]
  },

  #----------------------
print = function() {

    palette <- function(before, after, space)add_space_color(before, after, space, bgCyan$bold, bgCyan, bgMagenta)
    tables <- self$list_tables()
    if(all(tables %in% "")) {
      print_tables <- "[[empty db]]"
    } else {
      print_tables <- paste(tables, collapse = ", ")
    }

    print_obj_size <- paste0(self$get_metadata()$Robject_size,  " Kb")

    in_memory <- super$get_where()$data@dbname == ":memory:"
    if(in_memory) {
    print_db_size <- "[[in memory]]"
    } else {
    print_db_size <- paste0(self$get_metadata()$db_size, " Kb")
    }
    cat(bgCyan$bold("            dbR6 object                   \n"))
    cat("                                                              \n")
    cat(bgMagenta(" <-> ")); palette(" Data frames:", print_tables, 37); cat("\n");
    cat(bgMagenta(" <-> ")); palette(" Size of R object:", print_obj_size, 37); cat("\n");
    cat(bgMagenta(" <-> ")); palette(" Size of db on disk:", print_db_size, 37); cat("\n");

  },

  # poner un from----to y usar LIMITS de sql
get_table = function(what, from = NULL, to = NULL) {
    cond1 <- !is.null(from) && from <=0
    cond2 <- !is.null(to) && (to <=0 || to > self$nrow(what))
    if(cond1 || cond2) {
      stop("from and to must be >= 1, and to may not exceed the number of rows of the table")
    }
    if(is.null(from) && !is.null(to)) {
      limits <- paste0(" LIMIT ", to)
    } else if(!is.null(from) && is.null(to)) {
      limits <- paste0(" LIMIT -1 OFFSET ", from - 1)
    } else if(!is.null(from) && !is.null(to)) {
      limits <- paste0(" LIMIT ", to-from + 1," OFFSET ", from - 1)
    } else {
      limits <- ""
    }
    self$send_query(paste0("SELECT * FROM ", what, limits))
  },

  #---------------------
send_query = function(query) {
    this_query<-RSQLite::dbSendQuery(super$get_where()$data,  query)
    on.exit(RSQLite::dbClearResult(this_query))
    RSQLite::fetch(this_query)
  },

  #--------------------
send_statement = function(statement) {
    this_statement <- RSQLite::dbSendStatement(super$get_where()$data,  statement)
    on.exit(RSQLite::dbClearResult(this_statement))
  },

  #----------------------
add_table = function(new_name, new_df, overwrite = FALSE, append = FALSE) {
    if(new_name %in% self$list_tables() && !overwrite && !append) {
      stop("The table ", new_name, " exists in the working directory. Use overwrite = TRUE to overwrite it")
    }
    names <- self$get_metadata()$df_names
    self$set_metadata_single("df_names", c(names, new_name))
    RSQLite::dbWriteTable(super$get_where()$data, new_name, new_df, overwrite = overwrite, append = append)
    self$set_metadata()
    invisible(NULL)
  },

remove_table = function(what) {
    if(!what %in% self$list_tables()) return(paste0("Table '", what,   "' not found in database"))
      RSQLite::dbRemoveTable(super$get_where()$data, what)
      #this_statement <- RSQLite::dbSendStatement(super$get_where()$data,  paste0("DROP TABLE ", to_remove))
      #on.exit(RSQLite::dbClearResult(this_statement))
      self$set_metadata()
    invisible(NULL)
  },

  #--------------------
add_empty_table = function(new_name, shape, overwrite) {
    if(new_name %in% self$list_tables()) {
      if(!overwrite) {
        stop("The table ", new_name, " exists in the working directory. Use overwrite = TRUE to overwrite it")
      } else {
        self$remove_table(what = new_name)
      }
    }
    self$send_statement(paste0("CREATE TABLE ", new_name, " AS SELECT * FROM ", shape, " WHERE 1=2"))
    invisible(NULL)
  },

  #--------------------

save = function(to) {
    this_data <- super$get_where()$data
    if(this_data@dbname != ":memory:") stop("db already present on disk")
    db <- RSQLite::dbConnect(RSQLite::SQLite(), to)
    RSQLite::sqliteCopyDatabase(from = this_data, to = db)
    super$set_data(db)
    self$set_metadata()
    message("object saved on disk")
    self
  },

clone_db = function(to) {
    db <- RSQLite::dbConnect(RSQLite::SQLite(), to)
    RSQLite::sqliteCopyDatabase(from = super$get_where()$data, to = db)
    copy <- self$clone(deep = TRUE)
    copy$set_data(db)
    copy
  },

  #----------------------

streamer = function(input, output, my_fun = function(y) y , n = 1000) {

    force(x <- 1)
    force(n)
    force(output)

    # iterator
    fun <- function() {
      if(x == 1)  self$add_empty_table(output, shape = input,  overwrite =  TRUE)
      this_query <- self$send_query(paste0("SELECT * FROM ", input,
                                           " WHERE id IN (SELECT id FROM ", input, " WHERE ", "id >= ", x,
                                           " AND id < ", x + n, ")"))

      ## create temporal table, copy to db, move to output and remove in each cycle

      self$add_table(new_name = output, new_df = my_fun(this_query), append = TRUE)

      x_0 <- x
      nr <- nrow(this_query)
      x <<- x + n
      if(nr == 0 && x_0 != 1)
      {
        return(0)
      } else {
        return(x-1)
      }
    }

    j <- 1
    this_time <- system.time({
      while(j != 0) {
        j <- fun()
        if(j != 0) cat(paste0("Yield ", j, "\n"))
      }
    })
    cat("Process finished in ", this_time[3], " seconds")
    invisible(NULL)
  }

)
)

