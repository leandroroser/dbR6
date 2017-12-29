## dbR6 

This package defines the dbR6 class, an R6 class which offers methods for creation and manipulation of SQLite tables. During the construction of the object, a database is created in-memory or on-disk. Tables can then be added and manipulated with methods provided by the class.  


## Available methods

A list with the methods is available on <href="#" onclick='window.open("https://leandroroser.github.io/dbR6/reference/index.html");return false;'> this link</a>

### Example

```diff
# let's create a big table:
long_table <- matrix(sample(letters, 100000, replace = TRUE), 10000, 10)
write.table(long_table, "long_table.txt", quote = FALSE)

# create a new dbR6 object (on-disk)
data <- dbR6$new("output.sqlite")

# write the big table in the database using the reader package, which
# allows to read a matrix in chunks
data$write_matrix("long_table.txt", out_name = "long", chunksize = 1000)

# The show method returns information about the object:
data
```

![Interface](https://github.com/leandroroser/dbR6/raw/master/inst/extdata/dbR6.jpg)


```diff
# A second method for dataframes is provided using the native 
# read.table function, but it is slow for big tables

small_table <- matrix(sample(letters, 1000, replace = TRUE), 100, 10)
write.table(small_table, "small_table.txt", quote = FALSE)
data$write_dataframe("small_table.txt", "small")

# Please note that the first method is for matrix (i.e., all columns of the same type) and the
# second for data frames (the columns can be of different type). The first one is recommended when
# working with tables with a same type. The package reader uses a pointer to locate the next chunk,
# while read.table needs to read all the data up to the next chunk and skips those rows, which makes it
# slower with increasing the size of the data.

# Listing tables
data$list_tables()

# Sending a query
data$send_query("SELECT * FROM salida LIMIT 5;")

```
