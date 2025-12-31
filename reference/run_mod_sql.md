# Execute a query on a mod Database

Connects to a DuckDB database, evaluates a user-supplied sql query and
closes the database.

## Usage

``` r
run_mod_sql(mod_db, query)
```

## Arguments

- mod_db:

  Path to the DuckDB database file (e.g., \`"my_data.mod.db"\`).

- query:

  An sql query supported by the duckdb database framework.

## Value

a mod_db object to allow piping

## Examples

``` r
if (FALSE) { # \dontrun{
# Count the number of rows in the calls table
run_mod_sql(
  mod_db = "my_data.mod.db",
  query = "CREATE TABLE call_count AS SELECT COUNT(*) AS num_rows FROM calls;")
} # }
```
