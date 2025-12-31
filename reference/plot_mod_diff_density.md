# Plot Case vs Control Methylation Fractions with Density-Weighted Points

This function generates a scatter plot of methylation fractions in case
vs control samples from a modification DuckDB database table, with
optional density shading and threshold lines.

## Usage

``` r
plot_mod_diff_density(
  mod_db,
  table,
  thresh = c(0.2, 0.3),
  palette = "turbo",
  n_kde = 250
)
```

## Arguments

- mod_db:

  A path to the modification DuckDB database, or an open database
  connection object.

- table:

  A string specifying the table name within the database, containing
  \`mh_frac_case\` and \`mh_frac_control\` columns.

- thresh:

  A numeric vector of length 1 or 2 giving the positive and negative
  threshold values to draw as dashed red lines above and below the
  identity line (default is \`c(0.2, 0.3)\`). If \`NULL\`, no threshold
  lines are drawn.

- palette:

  A string specifying the viridis color palette for density shading
  (default is \`"turbo"\`). Other options include \`"viridis"\`,
  \`"plasma"\`, \`"cividis"\`, etc.

- size:

  A numeric value controlling point size in the plot (default is
  \`0.8\`).

## Value

Invisibly returns the closed database connection object. The plot is
printed to the current graphics device.

## Details

This function visualizes the relationship between \`mh_frac_case\` and
\`mh_frac_control\` methylation values by plotting them on a scatter
plot. It optionally enhances visualization with local point density
using the \`ggpointdensity\` package if available.

Features:

- If **ggpointdensity** is installed, point density is used to color
  points.

- If not installed, a fallback to plain transparent points is used.

- A solid black identity line (\`y = x\`) is added for reference.

- Optional threshold lines at ±\`thresh\` distance from the identity
  line are shown as red dashed lines.

The plot is limited to the \[0, 1\] range for both axes and uses a clean
classic theme.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_mod_diff_density("my_data/my.mod.db", table = "chr5", thresh = c(0.25, 0.35))
} # }
```
