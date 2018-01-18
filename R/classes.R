
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
    deep_clone = call_dbR6(dbR6_deep_clone, alist(name =, value =))
  ),

  public = list(
    initialize = call_dbR6(dbR6_initialize,
                           alist(filename = ":memory:",
                                 overwrite = FALSE,
                                 new_metadata = FALSE)),
    finalize = call_dbR6(dbR6_finalize),
    #----------------------------------------------------------------------

    # general db configuration and methods
    location = call_dbR6(dbR6_location),
    save = call_dbR6(dbR6_save, alist(to =)),
    clone_db = call_dbR6(dbR6_clone_db, alist(to =)),
    print = call_dbR6(dbR6_print),

    # Metadata configuration
    get_metadata = call_dbR6(dbR6_get_metadata),
    load_metadata = call_dbR6(dbR6_load_metadata),
    set_metadata = call_dbR6(dbR6_set_metadata),
    set_one_metadata_value = call_dbR6(dbR6_set_one_metadata_value,
                                       alist(name = NULL, value = NULL)),

    # Tables manipulation
    list_tables = call_dbR6(dbR6_list_tables),
    get_tables_number = call_dbR6(dbR6_get_tables_number),
    get_table = call_dbR6(dbR6_get_table,
                          alist(what =, from =, to =, has_rownames = TRUE)),
    add_table = call_dbR6(dbR6_add_table,
                          alist(new_name =, new_df =,
                                overwrite = FALSE, append = FALSE,
                                write_rownames = TRUE, fun =, ...=)),
    remove_table =  call_dbR6(dbR6_remove_table, alist(what =)),
    copy_table_structure = call_dbR6(dbR6_copy_table_structure,
                                     alist(new_name =, from =, overwrite =)),
    exists_table = call_dbR6(dbR6_exists_table, alist(what =)),

    # Indexing
    create_index = call_dbR6(dbR6_create_index,
                             alist(what =, column =, unique_index = FALSE, ...=)),
    drop_index = call_dbR6(dbR6_drop_index, alist(index =)),
    get_index = call_dbR6(dbR6_get_index),

    # Tables attributes
    colnames = call_dbR6(dbR6_colnames, alist(what =)),
    nrow = call_dbR6(dbR6_nrow, alist(what =)),
    ncol = call_dbR6(dbR6_ncol, alist(what =)),
    dim = call_dbR6(dbR6_dim, alist(what =)),

    # Query methods
    send_query = call_dbR6(dbR6_send_query, alist(query =)),
    send_statement = call_dbR6(dbR6_send_statement, alist(statement =)),
    send_transaction = call_dbR6(dbR6_send_transaction, alist(...=)),
    statement_chunk = call_dbR6(dbR6_statement_chunk, alist(what =, n =)),

    # Writig data
    streamer = call_dbR6(dbR6_streamer, alist(input =, output =, my_fun = function(y) y, n = 1000)),

     write_dataframe =  call_dbR6(dbR6_write_dataframe,
                                 alist(input =, output =,
                                       has_rownames = TRUE, has_colnames = TRUE,
                                       chunksize = 1000L, sep = " ",  columns_classes = character(0),
                                       autodetect = TRUE, scan_rows = 10, fun = NULL, ...=)),

    write_matrix =  call_dbR6(dbR6_write_matrix,
                              alist(input =, output =, has_rownames = TRUE, has_colnames = TRUE,
                                    chunksize = 1000L, sep = " ",
                                    fun = NULL, data_mod = "character")),


    # Table keys
    add_keys = call_dbR6(dbR6_add_keys, alist(key =, value =)),
    remove_keys = call_dbR6(dbR6_remove_keys, alist(key =)),
    get_keys = call_dbR6(dbR6_get_keys),

    # Operations
    sort = call_dbR6(dbR6_sort, alist(what =, column =, ...=)),
    filter = call_dbR6(dbR6_filter, alist(table =, conditions =, r_commands = FALSE)),
    cbind = call_dbR6(dbR6_cbind,
                      alist(t1=, ...=, outname=, join = c("left", "inner", "cross", "natural"),
                                        using_what=, overwrite = FALSE)),
    rbind = call_dbR6(dbR6_rbind, alist(outname =, union_type = c("union", "union_all"),
                                        overwrite = FALSE, remove_after = TRUE, ...=)),
    split = call_dbR6(dbR6_split, alist(x =, what =, remove_after = FALSE)),
    reduce = call_dbR6(dbR6_reduce,
                       alist(what =, union_type = c("union", "union_all"), remove_after = TRUE)),
    map_reduce = call_dbR6(dbR6_map_reduce,
                           alist(x =, what =, query_function =, overwrite = FALSE,
                                 remove_after = FALSE))
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

