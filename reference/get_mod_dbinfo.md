# Get Database Statistics

Prints out a summary of the mod database, including size, tables, and
unique sample names.

## Usage

``` r
get_mod_dbinfo(mod_db)
```

## Arguments

- mod_db:

  Path to the \`.mod.db\` file or a \`mod_db\` object.

## Value

Invisibly returns a list of stats from the database.

## Examples

``` r
 # Specify the path to the database
 mod_db <- system.file("my_data.mod.db", package = "MethylSeqR")
 
 # Get database statistics
 get_mod_dbinfo(mod_db = mod_db)
#> Error in get_mod_dbinfo(mod_db = mod_db): The database file  does not exist.
```
