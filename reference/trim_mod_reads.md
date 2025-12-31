# Trim methylation calls at the read ends in a mod database

Creates a new table in a mod database that keeps only methylation calls
within a central fraction of each read, optionally trimming from the
start, the end, or both. Trimming is expressed as a fraction of the read
length and applied via SQL filters on \`read_position\`.

## Usage

``` r
trim_mod_reads(
  mod_db,
  by_frac = 0.1,
  trim_start = FALSE,
  trim_end = TRUE,
  input_table = "calls",
  output_table = "calls_trimmed"
)
```

## Arguments

- mod_db:

  An object identifying the mod database, as accepted by
  `ModSeqR:::.modhelper_connectDB()` (e.g. a file path or an existing
  mod database object).

- by_frac:

  Numeric scalar between 0 and 1 (exclusive) giving the fraction of the
  read length to trim from each enabled end. For example,
  `by_frac = 0.1` trims 10% of the read length from the start and/or
  end.

- trim_start:

  Logical; whether to trim from the start (low `read_position`) of each
  read.

- trim_end:

  Logical; whether to trim from the end (high `read_position`) of each
  read.

- input_table:

  Character scalar; name of the input table containing methylation calls
  (default `"calls"`). Must contain at least `read_position` and
  `read_length` columns.

- output_table:

  Character scalar; name of the output table to create/overwrite with
  the trimmed calls (default `"calls_trimmed"`).

## Value

Invisibly returns the updated `mod_db` object, with `current_table` set
to `output_table`. The function is called for its side effects of
creating/replacing the trimmed table in the database.

## Details

For a read of length \`L\` and \`by_frac = f\`:

- If `trim_start = TRUE`, positions with `read_position < L * f` are
  dropped.

- If `trim_end = TRUE`, positions with `read_position >= L * (1 - f)`
  are dropped.

For example, with `read_length = 100` and `by_frac = 0.1`, positions
`0–9` and/or `90–99` are trimmed depending on which end(s) are enabled.

This function issues a `CREATE OR REPLACE TABLE` SQL statement against
the mod database. The resulting table contains a subset of rows from
`input_table` where the read positions fall within the untrimmed
interior region defined by `by_frac`, `trim_start`, and `trim_end`.

If `by_frac` is not strictly between 0 and 1, an error is thrown. If
both `trim_start` and `trim_end` are `FALSE`, the function also errors,
as there would be nothing to trim.

On success, `mod_db$current_table` is updated to `output_table` and a
short timing message is printed.

## Examples
