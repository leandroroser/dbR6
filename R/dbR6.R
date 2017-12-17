
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
#' \item{deep_clone}{lorem ipsum}
#' \item{initialize}{lorem ipsum}
#' \item{finalize}{lorem ipsum}
#' \item{get_where}{(inherited from R6_data class) lorem ipsum}
#' \item{set_data}{(inherited from R6_data class) lorem ipsum}
#' \item{get_metadata}{lorem ipsum}
#' \item{set_metadata}{lorem ipsum}
#' \item{set_metadata_single}{lorem ipsum}
#' \item{list_tables}{lorem ipsum}
#' \item{n_tables}{lorem ipsum}
#' \item{set_metadata}{lorem ipsum}
#' \item{colnames}{lorem ipsum}
#' \item{nrow}{lorem ipsum}
#' \item{ncol}{lorem ipsum}
#' \item{dim}{lorem ipsum}
#' \item{print}{lorem ipsum}
#' \item{get_table}{lorem ipsum}
#' \item{send_query}{lorem ipsum}
#' \item{send_statement}{lorem ipsum}
#' \item{add_table}{lorem ipsum}
#' \item{remove_table}{lorem ipsum}
#' \item{add_empty_table}{lorem ipsum}
#' \item{save}{lorem ipsum}
#' \item{clone_db}{lorem ipsum}
#' \item{sort}{lorem ipsum}
#' \item{create_index}{lorem ipsum}
#' \item{drop_index}{lorem ipsum}
#' \item{get_indices}{lorem ipsum}
#' \item{transaction}{lorem ipsum}
#' \item{filter}{lorem ipsum}
#' \item{exist_table}{lorem ipsum}
#' \item{statement_chunk}{lorem ipsum}
#' \item{streamer}{lorem ipsum}
#'  \item{read_write_in_chunks}{lorem ipsum}
#'
#' }
#' @export

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
    } else return(sort(out))
  },

n_tables = function() {
  out <- RSQLite::dbListTables(super$get_where()$data)
  length(out[out != "metadata"])
},

location = function() {
  super$get_where()$data@dbname
},

  #---------------------
colnames = function(what) {
    if(!what %in% self$list_tables()) return(paste0("Table '", what,   "' not found in database"))
    RSQLite::dbListFields(super$get_where()$data, what)
  },

  #--------------------
ncol = function(what) {
    if(!what %in% self$list_tables()) stop(paste0("Table '", what,   "' not found in database"))
    out <- RSQLite::dbListFields(super$get_where()$data, what)
    length(out)
  },

  #---------------------
nrow = function(what) {
    if(!what %in% self$list_tables()) stop(paste0("Table '", what,   "' not found in database"))
    out <- self$send_query(paste0("SELECT COUNT(*) FROM ", what))
    out[[1]]
  },

dim = function(what) {
  c(self$nrow(what), self$ncol(what))
  },

  #----------------------
print = function() {

  # detect EStudio session color (if using RStudio)
  col_bg<- try(rstudioapi::getThemeInfo()$dark, silent = TRUE)
  # patch for Dracula
  col_bg2 <-  try(rstudioapi::getThemeInfo()$editor == "Dracula", silent = TRUE)
  col_bg <- col_bg || col_bg2

  ### colors if object in R session###

  bgCol <- crayon::make_style("skyblue4", bg = TRUE)
  palette <- function(before, after, space)add_space_color(before, after, space,
                                                           crayon::bgCyan$white, crayon::bgCyan, crayon::bgMagenta$white)
  topCol <- bgCol$white
  if(!is.null(col_bg))
  {
    if(col_bg == "TRUE") {
    palette <- function(before, after, space)add_space_color(before, after, space,
                                                             crayon::bgCyan$black, crayon::bgCyan, crayon::bgMagenta$black)
    topCol <- bgCol$black
    }
  }

    tables <- self$list_tables()
    if(all(tables %in% "")) {
      print_tables <- " [[empty db]] "
    } else {
      print_tables <- paste(" ", tables, collapse = ", ")
      if(nchar(print_tables) > 33) {
      print_tables <- paste0(" ", substr(print_tables, 1, 17), " ... (", self$n_tables(), " tables) ")

      }
    }

    print_obj_size <- paste0(" ", self$get_metadata()$Robject_size,  " Kb ")

    in_memory <- super$get_where()$data@dbname == ":memory:"
    if(in_memory) {
    print_db_size <- " [[in memory]] "
    } else {
    print_db_size <- paste0(" ", self$get_metadata()$db_size, " Kb ")
    }

    print_location <- paste0(" ", self$location(), " ")
    if(nchar(print_location) > 33) {
      end <- nchar(print_location)
      start <- end - 30
      print_tables <- paste0(" ...", substr(print_location, start, nchar), "... ")

    }



    cat("\n")
    cat(topCol("                  dbR6 object                          "),  "\n\n")
    cat(crayon::bgMagenta(" <-> ")); palette(" Data frames: ", print_tables, 50); cat("\n")
    cat(crayon::bgMagenta(" <-> ")); palette(" Size of R object: ", print_obj_size, 50); cat("\n")
    cat(crayon::bgMagenta(" <-> ")); palette(" Size of db on disk: ", print_db_size, 50); cat("\n")
    cat(crayon::bgMagenta(" <-> ")); palette(" Location: ", print_location, 50); cat("\n")
    cat("                                                                      \n")
    invisible(self)
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
    RSQLite::dbGetRowsAffected(this_statement)
  },

  #----------------------
add_table = function(new_name, new_df, overwrite = FALSE, append = FALSE,
                     row.names = FALSE, ...) {
    if(new_name %in% self$list_tables() && !overwrite && !append) {
      stop("The table ", new_name, " exists in the working directory. Use overwrite = TRUE to overwrite it")
    }
    names <- self$get_metadata()$df_names
    self$set_metadata_single("df_names", c(names, new_name))
    RSQLite::dbWriteTable(super$get_where()$data, new_name, new_df, overwrite = overwrite,
                          append = append, row.names = row.names, ...)
    self$set_metadata()
    invisible(self)
  },

remove_table = function(what) {
    if(!what %in% self$list_tables()) return(paste0("Table '", what,   "' not found in database"))
      RSQLite::dbRemoveTable(super$get_where()$data, what)
      #this_statement <- RSQLite::dbSendStatement(super$get_where()$data,  paste0("DROP TABLE ", to_remove))
      #on.exit(RSQLite::dbClearResult(this_statement))
      self$set_metadata()
      invisible(self)
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
    invisible(self)
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
    invisible(self)
  },

clone_db = function(to) {
    db <- RSQLite::dbConnect(RSQLite::SQLite(), to)
    RSQLite::sqliteCopyDatabase(from = super$get_where()$data, to = db)
    copy <- self$clone(deep = TRUE)
    copy$set_data(db)
    invisible(copy)
  },

sort = function(what, column, ...) {
  dots <- as.character(unlist(list(...)))
  if(length(dots) > 0) column <- paste(c(column, dots), collapse = ", ")

  # > cost of time (create index before?)
  #self$send_statement(paste0("CREATE INDEX idx_temp ON ", what, " (", column, ")"))

  tempname <- paste("temp_", paste(sample(c(letters, 0:9, 20)), collapse = ""), sep = "")
  self$add_empty_table(tempname, what, overwrite = TRUE)
  self$send_statement(paste0("INSERT INTO ", tempname, " SELECT * FROM ", what, " ORDER BY ", column))
  self$remove_table(what)
  self$send_statement(paste0("ALTER TABLE ", tempname, " RENAME TO ", what))

  #self$send_statement("DROP INDEX IF EXISTS idx_temp") # but is previously removed when drop

  invisible(self)
},

create_index = function(what, column, unique = FALSE, ...) {
  dots <- as.character(unlist(list(...)))
  if(length(dots) > 0) column <- paste(c(column, dots), collapse = ", ")
  self$send_statement(paste0("CREATE ", ifelse(unique, "UNIQUE", ""),  " INDEX idx_temp ON ", what, " (", column, ")"))
  invisible(self)
},

drop_index = function(index) {
  self$send_statement(paste0("DROP INDEX IF EXISTS ", index))
  invisible(self)
},

get_indices = function() {
  self$send_query("SELECT name FROM sqlite_master WHERE type='index' ORDER BY name;")
},

# pass a list of unquoted arguments in transaction
# example:
# mylis <- list(a$transaction("CREATE TABLE t1(a, b PRIMARY KEY)"),
# a$transaction("CREATE TABLE t2(a, b PRIMARY KEY)", "DROP TABLE t1"))
# obj$transaction(mylist)


parse_transaction = function(...) {

  fun <- function(...) {
    args <- as.list(substitute(...))[-1]
    x<-lapply(args, function(x) as.expression(bquote(RSQLite::dbExecute(self$get_where()$data, .(x)))))
    as.expression(unlist(x))
  }

  what <- fun(...)
  RSQLite::dbWithTransaction(super$get_where()$data, eval(what))
  invisible(self)
},


# R commands to be evaluated in the condition ('where' query)
# are indicated witihin %rs& and %re% as in:  %rs% my_command %re%

filter = function(table, conditions, r_commands = FALSE) {

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
},

exist_table = function(name) {
  RSQLite::dbExistsTable(super$get_where()$data, name)
},

statement_chunk = function(what, n) {
  t_s <- RSQLite::dbSendStatement(super$get_where()$data, what)
  while (!RSQLite::dbHasCompleted(t_s)) {
   chunk <- RSQLite::dbFetch(t_s, n = n)
   print(nrow(chunk))
  }
  RSQLite::dbClearResult(t_s)
 invisible(self)
},


# TODO
# ---> sql constructor
# # create sql constructor able to create statement with:
# -> create, alter - rename to, drop
# -> insert, update - set, delete
# -> select
# -> where, limit -offset - between, distinct,  group by, having, order by,in
# -> operators, AND - ON, like, NOT, NOT-IN
# -> joins
#----> check if transactions are relevant to be added

# sql_constructor = function()


#----------------------

## it is assuming an id in the table, may be changed
streamer = function(input, output, my_fun = function(y) y , n = 1000) {

    force(x <- 1)
    force(n)
    force(output)

    # iterator
    iter_fun <- function() {
      if(x == 1)  self$add_empty_table(output, shape = input,  overwrite =  TRUE)
      this_query <- self$send_query(paste0("SELECT * FROM ", input,
                                           " WHERE id IN (SELECT id FROM ", input, " WHERE ", "id >= ", x,
                                           " AND id < ", x + n, ")"))

      ## move to output and remove in each cycle

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
        j <- iter_fun()
        if(j != 0) cat(paste0("Yield ", j, "\n"))
      }
    })
    cat("Process finished in ", this_time[3], " seconds")
    invisible(self)
}

#----------

read_write_in_chunks =  function(infile, out_name, my_db, header, chunksize, sep, fun = NULL,...) {

    skip <- 0

    if(header) {
      true_header <- as.character(read.table(infile, nrows  = 1, stringsAsFactors = FALSE)[1, ])
      skip <- 1
    }

    data <- read.table(infile,
                       header = FALSE,
                       sep = sep,
                       na.strings = "NA",
                       colClasses = c("character"),
                       strip.white = TRUE,
                       comment.char="",
                       stringsAsFactors = FALSE,
                       nrows = chunksize,
                       skip = skip,
                       row.names = 1,
                       ... )

    if(header) {
      colnames(data) <- true_header
    }
    skip <- skip + nrow(data)

    if(!is.null(fun)){
      data <- fun(data)
    }

    my_db$add_table(out_name, data, overwrite = TRUE )

    while(nrow(data) > 0) {

      tryCatch(data <- read.table(infile,
                                  header = header,
                                  sep = sep,
                                  na.strings = "NA",
                                  colClasses = c("character"),
                                  strip.white = TRUE,
                                  comment.char="",
                                  stringsAsFactors = FALSE,
                                  nrows = chunksize,
                                  skip = skip,
                                  row.names = 1,
                                  ... ),
               error = function(e) {
                 data <<-data.frame()
                 if(length(grep("no lines available in input", e$message) == 0)) {
                   print(e$message)
                   file.remove(out_name)
                 }
               })

      if(nrow(data) > 0) {
        if(header) {
          colnames(data) <- true_header
        }
        skip <- skip + nrow(data)

        if(!is.null(fun)){
          data <- fun(data)
        }

        my_db$add_table(out_name, data, append = TRUE )
      }
    }

    cat("Written ", skip, " lines into database")
    invisible(NULL)
  }

)
)

