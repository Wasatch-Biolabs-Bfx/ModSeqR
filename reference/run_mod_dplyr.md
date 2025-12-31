# Execute an Expression on a DuckDB Table with Optional Materialization

Connects to a DuckDB database, evaluates a user-supplied expression on a
specified table, and either collects the result into R or computes and
stores it as a new table in the database.

## Usage

``` r
run_mod_dplyr(
  mod_db,
  table_name,
  expr,
  mode = c("collect", "compute"),
  output_table = NULL
)
```

## Arguments

- mod_db:

  Path to the DuckDB database file (e.g., \`"my_data.mod.db"\`).

- table_name:

  Name of the table in the database to operate on.

- expr:

  A function taking one argument (\`tbl_ref\`) and returning a lazy
  \`dplyr\` expression. The table reference (\`tbl_ref\`) will be passed
  as a \`tbl()\` object connected to the database.

- mode:

  One of \`"collect"\` (default) or \`"compute"\`. If \`"collect"\`, the
  result is returned as a data frame in R. If \`"compute"\`, the result
  is stored as a new table in the database.

- output_table:

  Required if \`mode = "compute"\`. Name of the output table to create
  or overwrite with the result of \`expr(tbl_ref)\`.

## Value

If \`mode = "collect"\`, returns a data frame. If \`mode = "compute"\`,
returns \`NULL\` invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
# Collect results of a filtered table into R
run_mod_dplyr(
  mod_db = "my_data.mod.db",
  table_name = "methylation_data",
  expr = function(tbl_ref) dplyr::filter(tbl_ref, score > 0.5),
  mode = "collect"
)

# Store the filtered result in a new table inside the database
run_mod_dplyr(
  mod_db = "my_data.mod.db",
  table_name = "methylation_data",
  expr = function(tbl_ref) dplyr::filter(tbl_ref, score > 0.5),
  mode = "compute",
  output_table = "filtered_data"
)
} # }
```
