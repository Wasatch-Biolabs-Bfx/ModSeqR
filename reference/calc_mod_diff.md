# Calculate Differential Methylation

This function calculates differential methylation between specified case
and control groups using various statistical methods. The results are
stored in a DuckDB database for further analysis.

## Usage

``` r
calc_mod_diff(
  mod_db,
  call_type = "positions",
  output_table = NULL,
  cases,
  controls,
  mod_type = "mh",
  calc_type = NULL,
  temp_dir = tempdir(),
  threads = NULL,
  memory_limit = NULL,
  min_sites = NULL,
  min_cov = NULL,
  overwrite = TRUE
)
```

## Arguments

- mod_db:

  A list containing the database file path. This should be a valid
  "mod_db" class object.

- call_type:

  A string representing the name of the table in the database from which
  to pull the data. Default is "positions".

- output_table:

  Destination table name for results. If NULL, defaults to
  paste0("mod_diff\_", call_type).

- cases:

  A character vector containing the sample names for the case group.

- controls:

  A character vector containing the sample names for the control group.

- mod_type:

  A string indicating the type of modification to analyze. Default is
  "mh" for methylation/hydroxymethylation. Other codes include "a" for
  6mA, "17596" for inosine, and "17802" for pseudouridine. Bare numeric
  codes are automatically prefixed with "m\_".

- calc_type:

  A string specifying the statistical method to use. Options: "wilcox",
  "beta_bin", "fast_fisher", "r_fisher", "log_reg". Default is NULL, in
  which case:

  - "wilcox" if both groups have \>= 5 samples

  - "beta_bin" if both groups have \>= 2 samples (accounts for
    overdispersion)

  - "fast_fisher" if either group has only 1 sample

- temp_dir:

  Directory for DuckDB temporary files (default
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html)).

- threads:

  Integer DuckDB thread count. If `NULL`, an internal heuristic
  (typically all-but-one core) is used.

- memory_limit:

  DuckDB memory limit string (e.g. `"16384MB"`). If `NULL`, an internal
  heuristic (~80% of RAM) is used.

- min_sites:

  Minimum number of distinct modification sites (e.g., CpGs) required
  per sample within a window. Windows where any sample has fewer than
  this many sites with calls are dropped before testing. This filters
  out windows with poor breadth of coverage. Only applies when the input
  table contains a `num_sites` column (i.e., windows). Default is `NULL`
  (no filtering).

- min_cov:

  Minimum average coverage per modification site, estimated as
  `num_calls / num_sites` for each sample in each window. Windows where
  any sample falls below this threshold are dropped before testing. For
  example, `min_cov = 5` requires an average of at least 5 calls per CpG
  site per sample. Only applies when the input table contains both
  `num_calls` and `num_sites` columns. Default is `NULL` (no filtering).

- overwrite:

  If TRUE and output_table exists, it is dropped before writing.

## Value

A list containing the updated "mod_db" object with the latest tables in
the database, including "meth_diff".

## Details

The function connects to the specified DuckDB database and retrieves
methylation data from the specified call type table. It summarizes the
data for cases and controls, calculates p-values based on the specified
method, and stores the results in the "meth_diff" table. Resource
pragmas (`temp_directory`, `threads`, `memory_limit`) are set via
internal heuristics unless overridden.

## Examples
