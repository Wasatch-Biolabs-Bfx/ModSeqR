# Get Methylation Statistics from the Modifications Database

This function retrieves and calculates methylation statistics (mean
methylation fractions) from a specified table in the mod database. It
can either return summary statistics or plot a histogram of the
methylation values, depending on the user's preference.

## Usage

``` r
plot_mod_modfrac(
  mod_db,
  call_type = c("positions", "regions"),
  plot = TRUE,
  save_path = NULL,
  max_rows = NULL
)
```

## Arguments

- mod_db:

  A string. The path to the database containing mod files from nanopore
  data.

- call_type:

  A character vector specifying the type of data to retrieve from the
  database. Default is "positions". Can also be "regions".

- plot:

  A logical value. If TRUE, a histogram of methylation values is
  plotted. Default is FALSE.

- save_path:

  Pathway to save the plot to. Usually .pdf or .png.

- max_rows:

  The maximum amount of rows wanted for calculation. This argument can
  help analysis run faster when there is a lot of data.

## Value

If \`plot\` is FALSE, the function prints a summary of the methylation
statistics and quantiles. If \`plot\` is TRUE, it displays a histogram
of methylation values.

## Details

The function connects to the specified database, checks for the
existence of the relevant table, and retrieves methylation fraction
data. If the table contains methylation data, it prioritizes the
\`mh_frac\` column over others. Depending on the \`call_type\`, it can
compute statistics for either per base or per region. If \`plot\` is set
to TRUE, it generates a histogram of the methylation values.

## Note

The function assumes that the database has tables named according to the
\`call_type\` parameter (e.g., "positions", "regions"). It also expects
specific columns for methylation data to exist.

## Examples

``` r
 # Specify the path to the database
 mod_db <- system.file("my_data.mod.db", package = "ModSeqR")
 
 # Get methylation statistics for the 'positions' call type without plotting
 plot_mod_modfrac(mod_db = mod_db, call_type = "positions")
#> Error in plot_mod_modfrac(mod_db = mod_db, call_type = "positions"): positions Table does not exist in the database. Check spelling or make sure you create it first.
```
