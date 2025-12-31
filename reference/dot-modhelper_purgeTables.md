# Internal Function to Purge Unwanted Tables from Database

This internal function connects to a DuckDB database and removes tables
that are not specified in the \`keep_tables\` list. It retains only the
tables that you want to keep in the database.

## Usage

``` r
.modhelper_purgeTables(db_con)
```

## Arguments

- mod_db:

  A character string or an object of class \`mod_db\` representing the
  DuckDB database to connect to.

## Value

None. This function is called for its side effects (modifying the
database).

## Details

The function connects to the specified database, lists all tables, and
removes those not included in the \`keep_tables\` vector. After purging,
it prints the names of the remaining tables in the database.
