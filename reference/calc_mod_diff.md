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
  "mh" for methylation/hydroxymethylation.

- calc_type:

  A string specifying the statistical method to use for calculating
  p-values. Options include "wilcox", "fast_fisher", "r_fisher", and
  "log_reg". Default is NULL, in which case "wilcox" is used if there
  are replicates in either group, otherwise "fast_fisher" is used.

- overwrite:

  If TRUE and output_table exists, it is dropped before writing.

## Value

A list containing the updated "mod_db" object with the latest tables in
the database, including "meth_diff".

## Details

The function connects to the specified DuckDB database and retrieves
methylation data from the specified call type table. It summarizes the
data for cases and controls, calculates p-values based on the specified
method, and stores the results in the "meth_diff" table.

## Examples
