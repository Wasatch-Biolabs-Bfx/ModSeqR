# Count unique CpG sites in a mod database table

Computes the number of unique CpG sites (defined by distinct pairs of
`start` and `end` positions) in a table of calls within a mod database.
Accepts either a file path to a database or an existing mod database
object.

## Usage

``` r
get_mod_cpg_count(mod_db, table_name = "calls")
```

## Arguments

- mod_db:

  Character string giving the path to a mod database file, or an
  object/handle accepted by
  [`.modhelper_connectDB()`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/dot-modhelper_connectDB.md).

- table_name:

  Character scalar. Name of the table to query. Defaults to `"calls"`.

## Value

An integer (length-one) giving the number of unique CpG sites, returned
*invisibly*. If `table_name` does not exist, returns `NULL` invisibly
after printing a message. The function also prints a formatted summary
to the console as a side effect.

## Errors

If `mod_db` is a character path and the file does not exist, an error is
thrown with [`stop()`](https://rdrr.io/r/base/stop.html) before
attempting any connection.

## Examples

``` r
if (FALSE) { # \dontrun{
# Using a file path
n <- get_mod_cpg_count("/path/to/my.mod.db", table_name = "calls")
n

# Using a pre-opened handle (package-internal)
dbh <- .modhelper_connectDB("/path/to/my.mod.db")
get_mod_cpg_count(dbh)              # prints summary, returns value invisibly
} # }
```
