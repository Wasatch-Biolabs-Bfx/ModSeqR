# Get Table Information from mod Database

Provides a summary of a specified table within a \`.mod.db\` database,
including total number of records, sample-level record counts (if
applicable), and column names.

## Usage

``` r
get_mod_tableinfo(mod_db, table_name)
```

## Arguments

- mod_db:

  Path to the \`.mod.db\` file or a \`mod_db\` object.

- table_name:

  Name of the table to summarize.

## Value

Invisibly returns the closed \`mod_db\` object after summarizing the
specified table.

## Examples

``` r
# Specify the path to the database
mod_db <- system.file("my_data.mod.db", package = "ModSeqR")

# Get information about the 'calls' table
get_mod_tableinfo(mod_db = mod_db, table_name = "calls")
#> Error in get_mod_tableinfo(mod_db = mod_db, table_name = "calls"): The database file   does not exist.
```
