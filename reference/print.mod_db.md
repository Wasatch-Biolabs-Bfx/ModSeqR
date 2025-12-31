# Helper Function to Print Database Tables

This function connects to a DuckDB database and prints the names of all
tables currently in the database. It can also print the first few rows
of specific tables.

## Usage

``` r
# S3 method for class 'mod_db'
print(mod_db)
```

## Arguments

- mod_db:

  A character string or an object of class mod_db representing the
  DuckDB database to connect to.

- tables:

  A character vector specifying the names of tables to print. Defaults
  to the last table given, which will print all tables. If specific
  table names are provided, it will only print those (ex. "positions",
  "regions", "meth_diff"). If tables ="all", the function will print out
  all table sin the database...

## Value

None. This function is called for its side effects (printing
information).

## Details

The function establishes a connection to the database, retrieves the
list of tables, and prints their names. If the specified table exists,
it also prints the first few rows of that table. If a specified table
does not exist, a message is printed to indicate this.

## Examples

``` r
# Specify the path to the database
 mod_files <- system.file("test_data", package = "MethylSeqR")
 mod_db <- tempfile("example_db")
 
 # Print out tables in the database
 make_pos_db(mod_files, mod_db) |> print()
#> Error in make_pos_db(mod_files, mod_db): could not find function "make_pos_db"
 
```
