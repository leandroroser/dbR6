
#' dbR6 class
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom crayon bgCyan
#' @importFrom crayon bgMagenta
#' @importFrom crayon bold
#' @import reader
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @field where (inherited from R6_data class) Environment storing data & enclosing environment for metadata
#' @field metadata Environment storing object metadata
#' @section Methods:
#' \describe{
#'
#' \enumerate{
#\item{\strong{deep_clone}}{deep clone dbR6 object (internal function).
#\subsection{Parameters}{
#\code{\strong{name}} What to clone ("metadata", "where" (database))-
#
#\code{\strong{Value}} The name of the output.
#}
#}
#' \item{\bold{initialize}}{
#'
#' Initialize dbR6 object.
#' \emph{---Parameters---}

#' \subsection{}{
#' \itemize{
#' \item{\code{\strong{filename}} ":memory:" for to create an in-memory database,
#' or a name for an on-disk database.}
#' \item{\code{\strong{overwrite}} The name of the output.}
#' }
#' }
#' }
#'

#' \item{\bold{finalize}}{
#'
#' Finalize dbR6 object.}
#'
# \item{get_where}{(inherited from R6_data class). Get location of the database linked to a dbR6 object (Internal).}
#'
# \item{set_data}{(inherited from R6_data class). Set dbR6 database (Internal).}
#'
#'
#' \item{\bold{get_metadata}}{
#'
#' Get object metadata.}
#'
# \item{set_metadata}{Set object metadata. (Internal).}
# \item{set_one_metadata_value}{Set a single metadata field (Internal).}
#'
#'
#' \item{\bold{list_tables}}{
#'
#' List tables in database.}
#'
#'
#' \item{\bold{get_tables_number}}{
#'
#' Get number of tables.}
#'
#' \item{\bold{colnames}}{
#'
#' Get colnames of a table.
#'
#' \emph{---Parameters---}
#'
#' \subsection{}{
#' \itemize{
#' \item{\code{\strong{what}} Name of table.}
#' }
#' }
#' }
#'
#' \item{\bold{nrow}}{
#'
#' Get the number of rows of a table.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{what}} Name of table.}
#' }
#' }
#' }
#'
#' \item{\bold{ncol}}{
#'
#' Get the number of columns of a table.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{what}} Name of table.}
#' }
#' }
#' }
#'
#' \item{\bold{dim}}{
#'
#' Get dimension of a table.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{what}} Name of table.}
#' }
#' }
#' }
#'
#' \item{\bold{print}}{
#'
#' Print dbR6 object graphical interface.}
#'
#' \item{\bold{get_table}}{
#'
#' Get a table.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{what}} Name of the table}
#' \item{\code{\strong{from}} Row where start to read (index >= 1)}
#' \item{\code{\strong{to}} Row where fininish to read}
#' }
#' }
#' }
#'
#' \item{\bold{send_query}}{
#'
#' Send an SQL query.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{query}} Query to send}
#' }
#' }
#' }
#'
#' \item{\bold{send_statement}}{
#'
#' Send an SQL statement.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{
#' \code{\strong{statement}} Statement to send}
#' }
#' }
#' }
#'
#' \item{\bold{add_table}}{
#'
#' Add a table to a dbR6  object.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{new_name}} Name for the new table}
#'
#' \item{\code{\strong{new_df}} Input table}
#'
#' \item{\code{\strong{overwrite}} Overwrite if already exists? Default FALSE}
#'
#' \item{\code{\strong{append}} Append content if already exists? Default FALSE}
#'
#' \item{\code{\strong{has_rownames}} Row names present? Default TRUE}
#'
#' \item{\code{\strong{fun}} Function to apply to the table before writing it}
#'
#' \item{\code{\strong{...}} Additional parameters passed to RSQLite::dbWriteTable.}
#' }
#' }
#' }
#'
#' \item{\bold{remove_table}}{
#'
#' Remove a table.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{what}} Name of table.}
#' }
#' }
#' }
#'
#' \item{\bold{copy_table_structure}}{
#'
#' Add an empty table.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{new_names}} Name of the new table.}
#'
#' \item{\code{\strong{from}} Table to copy structure from.}
#'
#' \item{\code{\strong{overwrite}} Overwrite table if exists?.}
#' }
#' }
#' }
#'
#' \item{\bold{save}}{
#'
#' Save a dbR6 database on disk.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{to}} Name of the new table.}
#' }
#' }
#' }
#'
#' \item{\bold{clone_db}}{
#'
#' Clone a dbR6 object.
#'
#' \emph{---Parameters---}
#'
#' \subsection{}{
#' \itemize{
#' \item\code{\strong{to}} Name of the new table.}
#' }
#' }
#'
#' \item{\bold{sort}}{
#'
#' Sort a table.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#'  \itemize{
#' \item{\code{\strong{what}} Name of the table to sort.}
#'
#' \item{\code{\strong{column}} Column used to sort.}
#'
#' \item{\code{\strong{...}} Vector with other columns used to sort.}
#' }
#' }
#' }
#'
#' \item{\bold{create_index}}{
#'
#' Create an index for a table.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{what}} Name of the table to sort.}
#'
#' \item{\code{\strong{column}} Column used to Create index.}
#'
#' \item{\code{\strong{unique}} Create unique index? (Logical).}
#'
#' \item{\code{\strong{...}} Other aditional columns in a character vector to create the index.}
#' }
#' }
#' }
#'
#' \item{\bold{drop_index}}{
#'
#' Drop an index from a table.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{what}} Name of the index to drop.}
#' }
#' }
#' }
#'
#' \item{\bold{get_indices}}{
#'
#' Get indices for a table.}
#'
#' \item{\bold{send_transaction}}{
#'
#' Generate transaction with the tables.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{...}} Character vector with transactions to parse.}
#' }
#' }
#' }
#'
#' \item{\bold{filter}}{
#'
#' Filter a table using the given conditions.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{table}} Name of the table.}
#'
#' \item{\code{\strong{conditions}} Logical conditions.}
#'
#' \item{\code{\strong{r_commands}}  R commands to be evaluated in the condition ('where' query)
#' indicated witihin \%rs& and \%re\% as in:  \%rs\% my_command \%re\%}
#' }
#' }
#' }
#'
#' \item{exist_table}{Verify if a table exists (Logical).
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{what}} Name of the table.}
#' }
#' }
#' }
#'
#'
#' \item{\bold{statement_chunk}}{
#'
#' Executes a statement in chunks
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{what}} Statement.}
#'
#' \item{\code{\strong{n}} Chunk size.}
#' }
#' }
#' }
#'
#' \item{\bold{streamer}}{
#'
#' Apply a function for an imput data table using chunks, storing the output
#' into a new table.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{input}} Input table.}
#' \item{\code{\strong{output}} Output table.}
#' \item{\code{\strong{my_fun}} R function to apply.}
#' \item{\code{\strong{n}} Chunk size.}
#' }
#' }
#' }
#'
#'
#' \item{\bold{write_dataframe}}{
#'
#' Write an external dataframe into the database using chunks.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{input}} Input table.}
#' \item{\code{\strong{output}} Output table.}
#' \item{\code{\strong{has_colnames}} Colnames present? (Logical).}
#' \item{\code{\strong{chunksize}} Chunk size.}
#' \item{\code{\strong{sep}} Character separating cell elements (default = " ").}
#' \item{\code{\strong{fun}} Function to apply to the chunk before writing it to the database.}
#' \item{\code{\strong{...}} Additional parameters passed to read.table.}
#' }
#' }
#' }
#'
#' \item{\bold{write_matrix}}{
#'
#' Write an external matrix into the database using chunks.
#'
#' \emph{---Parameters---}

#' \subsection{}{
#'
#' \itemize{
#' \item{\code{\strong{input}} Input table.}
#' \item{\code{\strong{output}} Output table.}
#' \item{\code{\strong{has_colnames}} colnames present? Default TRUE.}
#' \item{\code{\strong{has_rownames}} rownames present? Default TRUE.}
#' \item{\code{\strong{my_fun}}  Function to apply to the chunk before writing it to the database.}
#' \item{\code{\strong{data_mode}} R mode of the input data ("integer", "logical", "character", "numerical").}
#' }
#' }
#' }
#'
#' }
#' }
#' @export

dbR6 <- R6::R6Class("dbR6",

inherit = dbR6_data,
private = list(
  keys = NULL,
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
initialize = function(filename = ":memory:", overwrite = FALSE, new_metadata = FALSE) {
    exists_metadata <- super$initialize(filename, overwrite)
    private$metadata <- new.env(parent = super$get_where(), hash = FALSE)
    if(exists_metadata) {
      metadata_path <- paste0(super$get_where()$data@dbname, "_metadata.rds")

      if(!(file.exists(metadata_path) && new_metadata)) {
        stop("No metadata found for object. New metadata without keys can be created using the parameter
                new_metadata = TRUE. Note that a splitted table can not be reduced with the reduce() method
                when the original metadata has been lost and you should do a manual reduction via rbind()
                (see the rbind() method documentation.")
      } else if(!file.exists(metadata_path) && new_metadata) {
        self$set_metadata()
      } else {
        self$load_metadata()
      }

    } else {
    self$set_metadata()
    }
  },

  #----------------------
  finalize = function() {
    super$finalize()
  },

  #---------------------
get_metadata = function() private$metadata,

  #---------------------

load_metadata = function() {
  # internal checkpoint for coding errors
  if(super$get_where()$data@dbname == ":memory:") stop("in memory file has not on-disk metadata")

  metadata_path <- paste0(super$get_where()$data@dbname, "_metadata.rds")
  this_metadata <- readRDS(metadata_path)

  self$set_one_metadata_value(df_names, this_metadata$df_names)
  self$set_one_metadata_value(db_size, this_metadata$db_size)
  self$set_one_metadata_value(Robject_size, this_metadata$Robject_size)
  private$keys <- this_metadata$keys
},


set_metadata = function() {

    this_data <- super$get_where()$data
    in_memory <- this_data@dbname == ":memory:"
    df_names <- self$list_tables()
    if(!in_memory) {
      db_size <- round(as.numeric(na.omit(file.size(super$get_where()$data@dbname))) / 1E3, 3)
    } else {
      db_size <- 0
    }

    Robject_size <-  round(as.numeric(pryr::object_size(self))/ 1E6, 3)

    # in db
    if(!in_memory) {
      saveRDS(list(df_names = df_names, db_size = db_size,
                   Robject_size = Robject_size, keys = private$keys),
                   paste0(super$get_where()$data@dbname, "_metadata.rds"), compress = TRUE)
    }

    # create an in-memory copy of metadata
    self$set_one_metadata_value(df_names, df_names)
    self$set_one_metadata_value(db_size, db_size)
    self$set_one_metadata_value(Robject_size, Robject_size)
  },

  #--------------------
set_one_metadata_value = function(name, value) {
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

get_tables_number = function() {
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
      print_tables <- paste(tables, " ", collapse = ", ")
      if(nchar(print_tables) > 33) {
      print_tables <- paste0(" ", substr(print_tables, 1, 17), " ... (", self$get_tables_number(), " tables) ")

      }
    }

    print_obj_size <- paste0(" ", self$get_metadata()$Robject_size,  " Mb ")

    in_memory <- super$get_where()$data@dbname == ":memory:"
    if(in_memory) {
    print_db_size <- " [[in memory]] "
    } else {
    print_db_size <- paste0(" ", self$get_metadata()$db_size, " Mb ")
    }

    print_location <- paste0(" ", gsub(".*/", ".../", self$location()), " ")
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
get_table = function(what, from = NULL, to = NULL, has_rownames = TRUE) {
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
    out <- self$send_query(paste0("SELECT * FROM ", what, limits))
    if(has_rownames) {
    out <- as_table_with_rownames(out)
    }
    out
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
                     write_rownames = TRUE, fun = NULL,  ...) {
    if(new_name %in% self$list_tables() && !overwrite && !append) {
      stop("The table ", new_name, " exists in the working directory. Use overwrite = TRUE to overwrite it")
    }
    names <- self$get_metadata()$df_names
    self$set_one_metadata_value("df_names", c(names, new_name))

    if(!is.null(fun)) {
      new_df <- fun(new_df)
    }

    RSQLite::dbWriteTable(super$get_where()$data, new_name, new_df, overwrite = overwrite,
                          append = append, row.names = write_rownames, ...)
    self$set_metadata()
    invisible(self)
  },

remove_table = function(what) {

  tables_names <- self$list_tables()
    for(this_table in what) {
    if(!this_table %in% tables_names) return(paste0("Table '", this_table,   "' not found in database"))
      RSQLite::dbRemoveTable(super$get_where()$data, this_table)
    }
      #this_statement <- RSQLite::dbSendStatement(super$get_where()$data,  paste0("DROP TABLE ", to_remove))
      #on.exit(RSQLite::dbClearResult(this_statement))
      self$set_metadata()
      invisible(self)
  },

  #--------------------
copy_table_structure = function(new_name, from, overwrite) {
    if(new_name %in% self$list_tables()) {
      if(!overwrite) {
        stop("The table ", new_name, " exists in the working directory. Use overwrite = TRUE to overwrite it")
      } else {
        self$remove_table(what = new_name)
      }
    }
    self$send_statement(paste0("CREATE TABLE ", new_name, " AS SELECT * FROM ", from, " WHERE 1=2"))
    invisible(self)
  },

  #--------------------

save = function(to) {
    to <- paste0(to, "sqlite")
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
  self$copy_table_structure(tempname, what, overwrite = TRUE)
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


send_transaction = function(...) {

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

exist_table = function(what) {
  RSQLite::dbExistsTable(super$get_where()$data, what)
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

# sql_constructor = function()


#----------------------

## it is assuming an id in the table, may be changed
streamer = function(input, output, my_fun = function(y) y , n = 1000) {

    force(x <- 1)
    force(n)
    force(output)

    # iterator
    iter_fun <- function() {
      if(x == 1)  self$copy_table_structure(output, from = input,  overwrite =  TRUE)
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
},

#----------

write_dataframe =  function(input, output, has_colnames = TRUE, has_rownames = TRUE,
                            chunksize = 100, sep = " ", fun = NULL,...) {

    lines_completed <- 0

    con <- file(description=input,open="r")


    if(has_rownames) {
      rownames_value = 1
    } else {
      rownames_value <- NULL
    }


    if(has_colnames)
    {
      true_header <- read.table(con,
                                nrows = 1,
                                header = FALSE,
                                sep = sep,
                                na.strings = "NA",
                                strip.white = TRUE,
                                comment.char="",
                                stringsAsFactors = FALSE,
                                row.names = NULL,
                                ...)
     true_header<- as.character(true_header[1, ])
    }

    data <- read.table(con,
               header = FALSE,
               sep = sep,
               na.strings = "NA",
               strip.white = TRUE,
               comment.char="",
               stringsAsFactors = FALSE,
               nrows = chunksize,
               row.names = rownames_value,
               ...)

    if(has_colnames) {
      colnames(data) <- true_header
    }

    self$add_table(output, data, overwrite = TRUE, write_rownames = has_rownames)
    lines_completed <- this_lines <- nrow(data)

    while(this_lines > 0) {

      tryCatch({
          data <- read.table(con,
                                  header = FALSE,
                                  sep = sep,
                                  na.strings = "NA",
                                  strip.white = TRUE,
                                  comment.char="",
                                  stringsAsFactors = FALSE,
                                  nrows = chunksize,
                                  row.names = rownames_value,
                                  ... )
               this_lines <- nrow(data)
               lines_completed <- lines_completed  +  this_lines
               if(this_lines > 0) {
                 if(has_colnames) {
                   colnames(data) <- true_header
                 }

                 if(!is.null(fun)){
                   data <- fun(data)
                 }

                 self$add_table(output, data, append = TRUE )
                 cat("Written ", lines_completed, " lines into database\n")
               }
    },
               error = function(e) {
                 data <<-data.frame()
                 this_lines <<- 0
                 # only pass when all the lines were read
                 if(length(grep("no lines available in input", e$message)) == 0) {
                   stop(e)
                 }
               })
    }
    close(con)
    invisible(NULL)
  },

#----------

write_matrix =  function(input, output, has_colnames = TRUE,
                         chunksize = 1000L, sep = " ", has_rownames = TRUE,
                         fun = NULL, data_mod = "character", ...) {


my_reader <- chunkR::reader(input, sep, has_colnames, has_rownames, chunksize)

    lines_written <- 0

    while(chunkR::next_chunk(my_reader)) {
      data <- chunkR::get_matrix(my_reader)

      if(data_mod != "character") {
        mode(data) <- data_mod
      }

      if(!is.null(fun)){
        data <- fun(data)
      }

      data <- chunkR::matrix2df(data)
      if(lines_written == 0) {
       self$add_table(output, data, overwrite = TRUE, ...)
      } else {
       self$add_table(output, data, append = TRUE, ...)
      }
      lines_written <- lines_written +  nrow(data)
      cat("Written ", lines_written, " lines into database \n")
    }

  invisible(NULL)
},



cbind = function(x,  t1, ..., outname, join = c("left", "inner", "cross", "natural"), using_what){
  table_list <- list(...)
  joinlist <- list()

  if(join == "left" || join == "inner") {

    if(join == "left") {
      jointype <- "INNER JOIN"
    } else {
      jointype <- "LEFT OUTER JOIN"
    }

    for(i in seq_along(table_list)) {
      joinlist[[j]] <- paste0(jointype, table_list[[i]],  "ON", t1, ".ID",  table_list[[i]], ".ID ")
    }
    my_query <- paste0("CREATE TABLE ", outname, " AS SELECT * FROM ", t1, " ", do.call("c", joinlist))
  } else if (join == "cross") {
    for(i in seq_along(table_list)) {
      joinlist[[j]] <- paste0("CROSS JOIN", table_list[[i]], " ")
    }
  } else {
    for(i in seq_along(table_list)) {
      joinlist[[j]] <- paste0("NATURAL JOIN", table_list[[i]],  " USING ", using_what )
  }
  my_query <- paste0("CREATE TABLE ", outname, " AS SELECT * FROM ", t1, " ", do.call("c", joinlist))
}

self$send_statement(my_query)

invisible(self)
},



rbind = function(outname, union_type = c("union", "union_all"), remove_after = TRUE, ...){

  union_type <- match.arg(union_type)
  table_list <- list(...)

  self$copy_table_structure(outname, table_list[[1]])

  for(table_to_append in table_list) {
    self$send_statement(paste0("INSERT INTO ", outname, " SELECT * FROM ", table_to_append))

    if(remove_after) {
        self$remove_table(table_to_append)
      }
  }
  invisible(self)
},


add_keys = function(key, value) {
private$keys[key] = list(value)
},

remove_keys = function(key) {
  which_key <- which(names(self$keys) == key)
  if(length(which_key)>0) self$keys <- self$keys[-which_key]
},

get_keys = function() {
  private$keys
},

split = function(x, what) {
  my_factor <- self$send_query(paste0("SELECT DISTINCT ",what, " FROM ", x))[, 1]

  statement_fun <- function(y) paste0("CREATE TABLE ", y, " AS SELECT * FROM ",
                                      x, " WHERE ", paste0(x, ".", what), " = ","'", my_factor[i], "'")
  for(i in seq_along(my_factor)) {
    this_table <- paste0(what, "_", my_factor[i])
    statement_fun(this_table)
    self$send_statement(my_statement)
  }
  self$add_keys(what, my_factor)
  invisible(self)
},

reduce = function(x, what,  union_type = c("union", "union_all"), remove_after = TRUE) {
  union_type <- match_arg(union_type)
  #tabnames <- self$list_tables()
  #which_tables <- tabnames[grep(what, tabnames)]
  which_tables <- keys[what]

  if(length(which_tables) == 0) {
    stop("name of variable do not exists. Check names with the method get_keys()")
  }

  self$rbind(union_type, remove_after, which_tables)
  self$remove_keys(what)
  invisible(self)
},

map_reduce = function(x, what, query_function) {
  self$split(x, what)
  for(table_to_reduce in self$keys[what]) {
    self$send_query("CREATE TABLE __my_temp_table__ AS", query_function(table_to_reduce))
    self$remove_table(table_to_reduce)
    self$send_statement("ALTER TABLE __my_temp_table__ RENAME TO ", table_to_reduce)
  }
  self$reduce(table_to_reduce, what, "union")
  invisible(self)
}
)
)
