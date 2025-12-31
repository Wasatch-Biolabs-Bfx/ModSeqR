# Plot Differential Methylation Volcano Raster

Creates a volcano-style raster plot of differential methylation results
using database-backed plotting via \`dbplot\`. This function connects to
a DuckDB database, retrieves the specified differential methylation
table, filters and transforms the data, and generates a raster plot with
\`meth_diff\` on the x-axis and \`-log10(p-value)\` on the y-axis.

## Usage

``` r
plot_mod_diff(mod_db, table)
```

## Arguments

- mod_db:

  A \`mod_db\` object or a character string representing the file path
  to a DuckDB database. The database must contain a table with
  differential methylation results.

- table:

  A character string specifying the name of the table in the database
  containing the differential methylation data. The table must contain
  at least the following columns: \`meth_diff\` and \`p_val\`.

## Value

Invisibly returns the \`mod_db\` object after closing the database
connection. The function prints the ggplot2 raster plot to the active R
graphics device.

## Details

The plot uses \`dbplot::dbplot_raster()\` to efficiently create a raster
visualization of large-scale methylation difference data. It applies a
log10 transformation to the \`p_val\` column and uses a color gradient
to show the density of observations in each bin. A message is printed to
indicate the time taken to generate the plot.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_mod_diff("my_methylation.mod.db", "mod_diff_windows")
} # }
```
