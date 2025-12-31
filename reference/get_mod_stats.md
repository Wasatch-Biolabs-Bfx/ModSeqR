# Compute Statistics for a Ch3 File

This function computes various statistics for a given Ch3 file stored in
Parquet format, including total calls, total reads, mean read length,
CpG coverage at different read thresholds, flag distributions, and
high-quality call counts based on probability and base quality
thresholds.

## Usage

``` r
get_mod_stats(
  ch3_file,
  log_file = NULL,
  min_reads = c(1, 5, 10, 15),
  call_prob_threshold = 0.9,
  base_qual_threshold = 10,
  silently = FALSE
)
```

## Arguments

- ch3_file:

  Character. Path to the Ch3 file in Parquet format.

- log_file:

  Character (optional). Path to a log file where the output will be
  written. If NULL, results are printed to the console.

- min_reads:

  Numeric vector. A set of thresholds for reporting CpG coverage at
  different minimum read counts. Default: `c(1, 5, 10, 15)`.

- call_prob_threshold:

  Numeric. The minimum modification probability to consider a
  high-confidence call. Default: `0.9`.

- base_qual_threshold:

  Numeric. The minimum base quality required to count as a high-quality
  call. Default: `10`.

- silently:

  Logical. If `TRUE`, suppresses console output. Default: `FALSE`.

## Value

A list containing:

- `num_calls`:

  Total number of modification calls in the file.

- `num_reads`:

  Total number of unique reads in the file.

- `cpg_coverage`:

  A matrix with CpG coverage counts at different `min_reads` thresholds.

- `flag_counts`:

  A data frame with the count and percentage of calls per flag value.

- `high_conf_calls`:

  A data frame with the count and percentage of calls with modification
  probability above `call_prob_threshold`.

- `high_qual_calls`:

  A data frame with the count and percentage of calls with base quality
  above `base_qual_threshold`.

- `avg_read_length`:

  The mean read length.

## Details

The function reads the Ch3 file as a dataset and computes basic
statistics about the calls and reads. It also evaluates CpG coverage
based on different read count thresholds, distributions of flag values,
and counts of high-quality calls based on user-defined probability and
quality thresholds.

If a `log_file` is provided, the results are written to it. Otherwise,
they are printed to the console unless `silently = TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
get_mod_stats("example.ch3.parquet")
get_mod_stats("example.ch3.parquet", log_file = "stats.log", silently = TRUE)
} # }
```
