

#' dbR6_data class
#' @name dbR6Parent
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom methods setOldClass
#' @importFrom stats na.omit
#' @importFrom utils read.table
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @field where environment storing data & enclosing environment for metadata
#' @section Methods:
#' \describe{
#' \item{initialize}{initialize method}
#' \item{finalize}{finalize method}
#' \item{get_where}{get environment with the database connection}
#' \item{set_data}{set database connection}
#' }
#'@rdname dbR6Parent
NULL


#' dbR6 class
#' @name dbR6
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom crayon bgCyan
#' @importFrom crayon bgMagenta
#' @importFrom crayon bold
#' @import chunkR
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
#' \item{\strong{filename} ":memory:" for to create an in-memory database,
#' or a name for an on-disk database.}
#' \item{\strong{overwrite} The name of the output.}
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
#' \item{\strong{new_name} Name for the new table}
#'
#' \item{\strong{new_df} Input table}
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
#' @example
#' {
#' library(dbR6)
#'
# Let's create a table of 1E6 rows x 100 columns:
#'con <- file("long_table.txt", open = "w")
#'header <- paste("C", 1:100, sep = "", collapse = " ")
#'writeLines(header, con)
#'row_index <- 1
#' for(i in 1:100) {
#'  long_table <- matrix(sample(letters, 1000000, replace = TRUE), 10000, 100)
#'  rownames(long_table) <- row_index : (i * 10000)
#'  row_index <- row_index + 10000
#'  write.table(long_table, con, quote = FALSE, append = TRUE, col.names = FALSE, row.names = TRUE)
#'  cat("Written ", i * 10000, " of 1E6 rows\n")
#'}
#'close(con)


#'# Create a new dbR6 object (on-disk) with the method "new". All the methods
#'# available for the dbR6 class, are accessible via: some_dbR6_object$name_of_method(parameters).
# In this case we will create an SQLite database on disk:
#' data_on_disk <- dbR6$new("output.sqlite")
#'
#' # Write the big matrix in the on-disk database. The dbR6 package uses the chunkR package
#' # (available on this GitHub repository, https://github.com/leandroroser/chunkR),
#' # which allows to read a matrix in chunks efficiently:
#' require("chunkR")
#' data_on_disk$write_matrix(input = "long_table.txt", output  = "long", chunksize = 10000)
#'
#'
#' # The show method returns information about the object:
#' data_on_disk
#'
#' Interface
#'
#' # Call some of the available methods:
#' data_on_disk$list_tables()  # list tables
#' data_on_disk$get_table("long", 1, 10)  # get values from the "long" table, from rows 1 to 10
#' data_on_disk$location()  # location of the database
#' data_on_disk$nrow("long") # number of rows of "long" table
#' data_on_disk$ncol("long") # number of columns of "long" table
#' data_on_disk$send_query("SELECT * FROM long LIMIT 5;") #send an SQL query
#'
#'
#' # Method to write data frames
#'
#' # Please note that the first method is for matrix (i.e., all columns of the same type)
#' # while the second for data frames (the columns can be of different type).
#' # The first one is recommended when
#' # working with tables with a same type of data, as it is faster.
#'
#' data_on_disk$write_dataframe("long_table.txt", "long_as_df", chunksize = 10000)
#'
#'
#' # List tables
#' #' data$list_tables()
#'
#' # Remove table "long"
#' data_on_disk$remove_table("long", "long_as_df")
#'
#' # See the object
#' data_on_disk
#'
#' Objects in-memory and reconnections to existing databases
#'
#' # In-memory are created passing the parameter ":memory:" to the constructor
#' data_in_memory <- dbR6$new(":memory:")
#'
#' # For reconnection to an existing object the database is passed as argument to the constructor
#' reconnection <- dbR6$new("output.sqlite")
#'}
#'@rdname dbR6
NULL
