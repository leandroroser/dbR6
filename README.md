## dbR6 

This package defines the dbR6 class, a system with methods for creation and manipulation of SQLite databases. During the construction of the object, a database is created in-memory or on-disk. Tables can then be added and manipulated with methods provided by the class.  


## Available methods

Description of methods is available on <a href = "https://leandroroser.github.io/dbR6/reference/index.html"> this link </a>

### Example

```diff

library(dbR6)

# Let's create a table of 1E6 rows x 100 columns:

con <- file("long_table.txt", open = "w")
header <- paste("C", 1:100, sep = "", collapse = " ")
writeLines(header, con)
row_index <- 1
for(i in 1:100) {
long_table <- matrix(sample(letters, 1000000, replace = TRUE), 10000, 100)
rownames(long_table) <- row_index : (i * 10000)
row_index <- row_index + 10000
write.table(long_table, con, quote = FALSE, append = TRUE, col.names = FALSE, row.names = TRUE)
cat("Written ", i * 100000, " of 1E6 rows\n")
}
close(con)


# Create a new dbR6 object (on-disk) with the method "new". All the methods
# available for the dbR6 class, are accessible via: some_dbR6_object$name_of_method(parameters).
# In this case we will create an SQLite database on disk:
data_on_disk <- dbR6$new("output.sqlite")

# Write the big matrix in the on-disk database. The dbR6 package uses the chunkR package
# (available on this GitHub repository, https://github.com/leandroroser/chunkR),
# which allows to read a matrix in chunks efficiently:
require("chunkR")
data_on_disk$write_matrix(input = "long_table.txt", output  = "long", chunksize = 100000)


# The show method returns information about the object:
data_on_disk
```

![Interface](https://github.com/leandroroser/dbR6/raw/master/inst/extdata/dbR6.jpg)


```diff

# Call some of the available methods:
data_on_disk$list_tables()  # list tables
data_on_disk$get_table("long", 1, 10)  # get values from the "long" table, from rows 1 to 10
data_on_disk$location()  # location of the database
data_on_disk$nrow("long") # number of rows of "long" table
data_on_disk$ncol("long") # number of columns of "long" table
data_on_disk$send_query("SELECT * FROM long LIMIT 5;") #send an SQL query


# Method to write data frames

# Please note that the first method is for matrix (i.e., all columns of the same type) 
# while the second for data frames (the columns can be of different type). 
# The first one is recommended when
# working with tables with a same type of data, as it is faster.

data_on_disk$write_dataframe("long_table.txt", "long_as_df", chunksize = 10000)


# List tables
data$list_tables()

# Remove table "long"
data_on_disk$remove_table("long", "long_as_df")

# See the object
data_on_disk
```

### Objects in-memory and reconnections to existing databases

```diff

# In-memory are created passing the parameter ":memory:" to the constructor
data_in_memory <- dbR6$new(":memory:")

# For reconnection to an existing object the database is passed as argument to the constructor
reconnection <- dbR6$new("output.sqlite")

```
