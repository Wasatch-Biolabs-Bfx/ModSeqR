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
#> duckdb is keeping downloaded extensions in a temporary directory:
#> ℹ /tmp/RtmpV9o0kg/duckdb/extensions
#> This is removed when the R session ends, so extensions are re-downloaded each session.
#> ℹ To keep them, point `options(duckdb.extension_directory =)` or the `DUCKDB_EXTENSION_DIRECTORY` environment variable at a permanent path.
#> Table 'positions' does not exist in the database.
```
