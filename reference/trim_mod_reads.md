# Trim methylation calls at the read ends in a mod database

Creates a new table in a mod database that keeps only methylation calls
within a central fraction of each read (i.e., trims a fraction from the
start and/or end of each read).

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
  read length to trim from each enabled end. For example,
  `by_frac = 0.1` trims the first/last 10 on `trim_start`/`trim_end`.

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
  all samples. If non-NULL, requires a `sample_name` column in
  `input_table`. Only these samples are trimmed; all other samples are
  copied through unchanged.

## Value

Invisibly returns the updated `mod_db` object, with `current_table` set
to `output_table`.

## Details

If `samples` is `NULL` (default), trimming is applied to ALL samples. If
`samples` is provided, trimming is applied ONLY to those sample(s), and
ALL rows from other samples are retained unchanged in the output table.

## Examples

``` r
if (FALSE) { # \dontrun{
# Trim last 10% of reads for all samples
trim_mod_reads(mod_db, by_frac = 0.1, trim_end = TRUE, output_table = "calls_trimmed")

# Trim last 10% of reads only for Alzheimers2, keep all other samples intact
trim_mod_reads(mod_db, by_frac = 0.1, trim_end = TRUE,
               samples = "Alzheimers2", output_table = "calls_trimmed_A2")

# Trim both ends (5% each) for a subset of samples
trim_mod_reads(mod_db, by_frac = 0.05, trim_start = TRUE, trim_end = TRUE,
               samples = c("Control1", "Control2"), output_table = "calls_trimmed_controls")
} # }
```
