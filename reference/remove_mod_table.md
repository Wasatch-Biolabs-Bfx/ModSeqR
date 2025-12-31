# Remove a table from a database connection.

This function checks if a table exists and, if so, removes it. It
provides messages about the outcome.

## Usage

``` r
remove_mod_table(mod_db, table_name)
```

## Arguments

- mod_db:

  A database object.

- table_name:

  A string specifying the name of the table to remove.

## Value

Invisibly returns the database with the removed table.
