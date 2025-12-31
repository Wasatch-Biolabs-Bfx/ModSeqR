# Quality Control Wrapper for Methylation Data

This function serves as a wrapper for various quality control analyses
on methylation data. It sequentially calculates coverage statistics,
modification statistics, correlation analysis, and performs principal
component analysis (PCA).

## Usage

``` r
run_mod_qc(
  mod_db,
  call_type = "positions",
  plot = TRUE,
  max_rows = NULL,
  value = m_frac
)
```

## Arguments

- mod_db:

  A database connection or object containing methylation data.

- call_type:

  A character string indicating the type of call to retrieve data (e.g.,
  "positions", "regions").

- plot:

  Logical; whether to generate plots. Defaults to TRUE.

- max_rows:

  Optional maximum number of rows to use (sampling) for speed on large
  datasets.

- value:

  Column to use for analyses that take a value input (e.g.,
  correlations/PCA). Accepts a bare column name or a string. Default:
  m_frac.

## Value

Invisibly returns nothing; produces plots and console output.

## Examples

``` r
if (FALSE) { # \dontrun{
 mod_db <- system.file("my_data.mod.db", package = "MethylSeqR")
 run_mod_qc(mod_db, call_type = "positions")                 # uses m_frac
 run_mod_qc(mod_db, call_type = "regions", value = mh_frac)  # use mh_frac
} # }
```
