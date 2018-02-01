
# Define global variables for R check

globals <- c("self", "filename", "overwrite", "private", "x", "key", "value",
             "new_name", "fun", "data_mod", "fun", "output", "Robject_size",
             "autodetect", "chunksize", "columns_classes", "data_mod", "data_on_disk",
             "db_size", "df_names", "filename", "from", "fun", "has_colnames",
             "has_rownames", "index", "input", "j", "key", "my_fun", "n",
             "name", "new_metadata", "new_name", "outname", "output", "overwrite",
             "query", "query_function", "r_commands", "remove_after", "scan_rows",
             "sep", "statement", "tabname", "this_table", "to", "unique_index", "value",
             "what", "write_rownames", "x")

if(getRversion() >= "2.15.1")  utils::globalVariables(globals)

#' @rdname dbR6Parent
#' @export

dbR6Parent <- R6::R6Class("dbR6Parent",

  private = list(
    where = NULL
  ),

  public = list(
    initialize = call_dbR6(dbR6Parent_initialize,
                           alist(filename = ":memory:", overwrite = FALSE)),
    finalize = call_dbR6(dbR6Parent_finalize),
    #----------------------------------------------------------------------

    get_where = call_dbR6(dbR6Parent_get_where),
    set_data = call_dbR6(dbR6Parent_set_data, alist(x =)),
    set_metadata = call_dbR6(dbR6Parent_set_metadata, alist(x =))
  )
)

#'@rdname dbR6
#' @export

dbR6 <- R6::R6Class("dbR6",
  inherit = dbR6Parent,

  private = list(
    keys = NULL,
    metadata = NULL,
    metadata_path = NULL,
    valid = TRUE,
    deep_clone = call_dbR6(dbR6_deep_clone, alist(name =, value =)),
    validate_db = call_dbR6(dbR6_validate_db),

    load_metadata = call_dbR6(dbR6_load_metadata),
    set_metadata = call_dbR6(dbR6_set_metadata),
    set_one_metadata_value = call_dbR6(dbR6_set_one_metadata_value,
                                       alist(name = NULL, value = NULL)),
    # Table keys
    add_keys = call_dbR6(dbR6_add_keys, alist(key =, value =)),
    get_keys = call_dbR6(dbR6_get_keys),
    remove_keys = call_dbR6(dbR6_remove_keys, alist(key =))
  ),

  public = list(
    initialize = call_dbR6(dbR6_initialize,
                           alist(filename = ":memory:",
                                 overwrite = FALSE,
                                 new_metadata = FALSE)),
    finalize = call_dbR6(dbR6_finalize),
    delete_db = call_dbR6(dbR6_delete_db),
    is_valid = call_dbR6(dbR6_is_valid),
    #----------------------------------------------------------------------

    # general db configuration and methods
    clone_db = call_dbR6(dbR6_clone_db, alist(to =)),
    location = call_dbR6(dbR6_location),
    print = call_dbR6(dbR6_print),
    save = call_dbR6(dbR6_save, alist(to =)),

    # Metadata configuration
    get_metadata = call_dbR6(dbR6_get_metadata),

    # Tables manipulation
    add_table = call_dbR6(dbR6_add_table,
                          alist(from = , to =,
                                overwrite = FALSE,
                                append = FALSE,
                                fun =NULL,
                                index_row_names = TRUE,
                                ...=)
                          ),
    copy_table = call_dbR6(dbR6_copy_table, alist(from =, to =, overwrite = TRUE)),
    copy_table_structure = call_dbR6(dbR6_copy_table_structure,
                                     alist(from =, to =,  overwrite =)),
    exists_table = call_dbR6(dbR6_exists_table, alist(tabname =)),
    get_tables_number = call_dbR6(dbR6_get_tables_number),
    get_table = call_dbR6(dbR6_get_table,
                          alist(tabname =, start = NULL, end = NULL, has_rownames = TRUE)),

    list_tables = call_dbR6(dbR6_list_tables),

    remove_table =  call_dbR6(dbR6_remove_table, alist(tabname =)),


    # Indexing
    colnames = call_dbR6(dbR6_colnames, alist(tabname =)),
    create_index = call_dbR6(dbR6_create_index,
                             alist(tabname =, column =,
                                   index_name=,
                                   unique_index = FALSE,
                                   ...=)),
    drop_index = call_dbR6(dbR6_drop_index, alist(index =)),
    list_indices = call_dbR6(dbR6_list_indices),

    # Tables attributes
    dim = call_dbR6(dbR6_dim, alist(tabname =)),
    nrow = call_dbR6(dbR6_nrow, alist(tabname =)),
    ncol = call_dbR6(dbR6_ncol, alist(tabname =)),

    # Query methods
    send_query = call_dbR6(dbR6_send_query, alist(query =)),
    send_statement = call_dbR6(dbR6_send_statement, alist(statement =)),
    send_transaction = call_dbR6(dbR6_send_transaction, alist(...=)),
    statement_chunk = call_dbR6(dbR6_statement_chunk, alist(tabname =, n =)),

    # Writig data
    streamer = call_dbR6(dbR6_streamer, alist(from =, to =, my_fun = function(y) y, n = 1000)),

    write_dataframe =  call_dbR6(dbR6_write_dataframe,
                                 alist(from =, to =,
                                       has_rownames = TRUE,
                                       has_colnames = TRUE,
                                       chunksize = 1000L, sep = " ",
                                       columns_classes = character(0),
                                       autodetect = TRUE,
                                       scan_rows = 10,
                                       fun = NULL,
                                       index_row_names = TRUE,
                                       overwrite = FALSE,
                                       ...=)),

    write_matrix =  call_dbR6(dbR6_write_matrix,
                              alist(from =,
                                    to =,
                                    has_rownames = TRUE,
                                    has_colnames = TRUE,
                                    chunksize = 1000L,
                                    sep = " ",
                                    fun = NULL,
                                    data_mod = "character",
                                    index_row_names = TRUE,
                                    overwrite = FALSE,
                                    ...=)),

    # Operations
    cbind = call_dbR6(dbR6_cbind,
                      alist(to=, using_what= "row_names", ...=,
                            join = c("left", "inner", "cross", "natural"),
                            overwrite = FALSE)),
    filter = call_dbR6(dbR6_filter, alist(tabname =, conditions =, eval_before = FALSE)),
    map_reduce = call_dbR6(dbR6_map_reduce,
                           alist(from =, column =, query_function =, overwrite = FALSE,
                                 remove_after = FALSE)),
    rbind = call_dbR6(dbR6_rbind, alist(to =,  ...=,
                                        overwrite = FALSE,
                                        remove_appended = c("none", "sequential", "after"))),
    reduce = call_dbR6(dbR6_reduce,
                       alist(from =, to =)),
    sort = call_dbR6(dbR6_sort, alist(tabname =, column =, ...=)),
    split = call_dbR6(dbR6_split, alist(from=, to=,column=, remove_after = FALSE))

   )
)



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

