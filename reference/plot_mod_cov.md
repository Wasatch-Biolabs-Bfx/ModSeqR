# Calculate and Plot Coverage Statistics

This function calculates and optionally plots statistics for coverage
data from methylation sequencing experiments. It can handle both
positional and regional methylation data.

## Usage

``` r
plot_mod_cov(
  mod_db,
  call_type = "positions",
  plot = TRUE,
  save_path = NULL,
  max_rows = NULL
)
```

## Arguments

- mod_db:

  A data base either linking to the file name or of class mod_db.

- call_type:

  Either positions or regions data to analyze coverage on.

- plot:

  Logical, if `TRUE`, the function will generate a histogram of the
  coverage data. Default is `FALSE`.

- save_path:

  Pathway to save the plot to. Usually .pdf or .png.

- max_rows:

  The maximum amount of rows wanted for calculation. This argument can
  help analysis run faster when there is a lot of data.

## Value

If `plot` is `FALSE`, the function prints summary statistics and
percentiles of the coverage data. If `plot` is `TRUE`, it prints a
histogram of the log-transformed coverage data.

## Examples

``` r
 # Specify the path to the database
 mod_db <- system.file("my_data.mod.db", package = "ModSeqR")
 
 # Get coverage statistics for the 'positions' call type without plotting
 plot_mod_cov(mod_db = mod_db, call_type = "positions")
#> Error in plot_mod_cov(mod_db = mod_db, call_type = "positions"): positions Table does not exist in the database. Check spelling or make sure you create it first.
```
