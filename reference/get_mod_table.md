# Collect Table from DuckDB Database as Tibble

This function connects to a DuckDB database and collects a specified
table as a tibble.

## Usage

``` r
get_mod_table(mod_db, table_name, max_rows = NULL)
```

## Arguments

- mod_db:

  A list containing the database file path. This should be a valid
  "mod_db" class object.

- table_name:

  A string representing the name of the table to collect from the
  database.

- max_rows:

  The maximum amount of rows wanted for calculation. This argument can
  help analysis run faster when there is a lot of data.

## Value

A tibble containing the collected data from the specified database
table. If the table retrieval fails, an empty tibble is returned.

## Details

The function establishes a connection to the DuckDB database using
`.helper_connectDB`. It retrieves the specified table as a tibble. If an
error occurs during table retrieval, a message with the error is
displayed. The database connection is closed after retrieving the data,
regardless of success or failure.

## Examples

``` r
# Assuming mod_db is a valid database object and "positions" is a table in the database
mod_db <- system.file("my_data.mod.db", package = "ModSeqR")
positions = get_mod_table(mod_db, "positions")
#> Table 'positions' does not exist in the database.
```
