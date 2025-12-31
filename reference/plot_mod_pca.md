# Perform PCA on Methylation Data

This function performs Principal Component Analysis (PCA) on methylation
(or other) data retrieved from a DuckDB database. It aggregates the
chosen value column based on the specified call type and prepares it for
PCA analysis.

## Usage

``` r
plot_mod_pca(
  mod_db,
  call_type = "positions",
  value = m_frac,
  save_path = NULL,
  max_rows = NULL
)
```

## Arguments

- mod_db:

  A list containing the database file path. This should be a valid
  "mod_db" class object.

- call_type:

  A string representing the name of the table in the database from which
  to pull the data. Default is "positions".

- value:

  Column to use as the measurement for PCA (e.g., \`mh_frac\`,
  \`m_frac\`). Accepts a bare column name or a single string. Default:
  \`m_frac\`.

- save_path:

  Path to save the plot (e.g., .pdf or .png). If NULL, the plot is not
  saved.

- max_rows:

  Optional maximum number of rows to sample from the table (for speed on
  large datasets).

## Value

Produces a PCA plot (PC1 vs PC2) and prints a PCA summary and the PCA
scores.

## Details

The function connects to the specified DuckDB database, retrieves data
from \`call_type\`, reshapes to a features × samples matrix (features
are regions/windows/positions; columns are samples), scales features,
then runs PCA on samples (transpose before \`prcomp\`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Default (m_frac)
 plot_mod_pca(mod_db)
 # Use mh_frac instead
 plot_mod_pca(mod_db, call_type = "regions", value = mh_frac)
 # Or as a string
 plot_mod_pca(mod_db, call_type = "windows", value = "mh_frac")
} # }
```
