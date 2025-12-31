# Correlation Matrix of Modified Sequence Data

This function calculates and optionally plots a correlation matrix for
methylation or other modification fraction data from genomic positions.
It supports position-based, region-based, and window-based calls and
provides visualization using ggplot2.

## Usage

``` r
calc_mod_samplecor(
  mod_db,
  call_type = c("positions"),
  value = m_frac,
  agg_fun = mean,
  plot = TRUE,
  save_path = NULL,
  plot_sample_order = NULL,
  plot_title = "Sample Correlation Matrix",
  max_rows = NULL
)
```

## Arguments

- mod_db:

  A string. The path to the database containing ch3 files from nanopore
  data.

- call_type:

  A character vector specifying the type of data to retrieve from the
  database. Options are "positions", "regions", or "windows". Default is
  "positions".

- value:

  Column in the table to use as the measurement (e.g., mh_frac, m_frac).
  Accepts a bare column name or a string. Default: mh_frac.

- agg_fun:

  Function used to aggregate when multiple rows map to the same cell
  during pivot (e.g., duplicates in regions or windows). Default: mean.

- plot:

  Logical. If TRUE, plot a correlation heatmap. Default: TRUE.

- save_path:

  Optional file path to save the plot.

- plot_sample_order:

  Optional character vector setting sample order in the heatmap.

- plot_title:

  Title for the heatmap.

- max_rows:

  Optional integer to randomly sample rows from the table for faster
  runs.

## Value

Invisibly returns the connection wrapper; prints the correlation matrix
and (optionally) a ggplot heatmap.

## Details

The function connects to the mod database, pulls data for the selected
\`call_type\`, reshapes to a samples x samples matrix, and computes
Pearson correlations using \`use = "pairwise.complete.obs"\`. The
\`value\` column controls which measurement is used.

## Examples

``` r
if (FALSE) { # \dontrun{
# Use mh_frac (default)
calc_mod_samplecor(mod_db = "my_data.mod.db", call_type = "positions")
# Use m_frac
calc_mod_samplecor(mod_db = "my_data.mod.db", call_type = "regions", value = m_frac)
# Or as a string
calc_mod_samplecor(mod_db = "my_data.mod.db", call_type = "windows", value = "m_frac")
} # }
```
