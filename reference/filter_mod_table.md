# Filter a DuckDB Table and Save to a New Table

Applies filter conditions to a table in a DuckDB database and saves the
result to a new or replaced output table. This is done lazily using
\`dbplyr\`, so the filtering is translated to SQL and executed inside
the database (not in R).

## Usage

``` r
filter_mod_table(mod_db, input_table, output_table, ...)
```

## Arguments

- mod_db:

  Path to the DuckDB database file (e.g., \`"my_data.mod.db"\`).

- input_table:

  Name of the table to filter.

- output_table:

  Name of the output table to create or overwrite with the filtered
  results.

- ...:

  Filtering expressions (e.g., \`score \> 0.5\`, \`gene_id == "abc"\`).
  These are unquoted expressions passed directly to \`dplyr::filter()\`.

## Value

Invisibly returns the path to the DuckDB file (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
filter_mod_table(mod_db = train_db, 
input_table = "collapsed_windows", 
output_table = "collapsed_windows",
!(chrom %in% c("chrX", "chrY")))
} # }
```
