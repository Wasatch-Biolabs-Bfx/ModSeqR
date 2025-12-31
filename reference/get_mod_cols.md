# Get Column Names from a DuckDB Table

Returns a character vector of column names from a specified table in a
DuckDB database file.

## Usage

``` r
get_mod_cols(mod_db, table_name)
```

## Arguments

- mod_db:

  Path to the DuckDB database file (e.g., \`"my_data.mod.db"\`).

- table_name:

  Name of the table whose column names are to be retrieved.

## Value

A character vector of column names from the specified table.

## Examples

``` r
if (FALSE) { # \dontrun{
get_mod_cols("my_data.mod.db", "windows")
} # }
```
