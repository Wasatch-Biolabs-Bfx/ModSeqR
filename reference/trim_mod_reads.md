# Trim methylation calls at the read ends in a mod database

Creates a new table in a mod database that keeps only methylation calls
within a central fraction of each read. Optionally restricts trimming to
one or more sample(s) via the `samples` argument.

## Usage

``` r
trim_mod_reads(
  mod_db,
  by_frac = 0.1,
  trim_start = FALSE,
  trim_end = TRUE,
  input_table = "calls",
  output_table = "calls_trimmed",
  samples = NULL
)
```

## Arguments

- mod_db:

  An object identifying the mod database, as accepted by
  `ModSeqR:::.modhelper_connectDB()` (e.g. a file path or an existing
  mod database object).

- by_frac:

  Numeric scalar between 0 and 1 (exclusive) giving the fraction of the
  read length to trim from each enabled end.

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

- samples:

  Character vector of sample name(s) to trim. If `NULL` (default), trims
  all samples. Requires a `sample_name` column in `input_table`.

## Value

Invisibly returns the updated `mod_db` object, with `current_table` set
to `output_table`.
