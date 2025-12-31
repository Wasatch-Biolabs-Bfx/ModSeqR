# Collapse Windows Based on Methylation Differences

This function collapses significant windows in a methylation dataset by
merging contiguous regions that meet the specified criteria. Can only
collapse windows once a differential modification analysis
(calc_mod_diff()) has been called.

## Usage

``` r
collapse_mod_windows(
  mod_db,
  table_name = "collapsed_windows",
  max_distance = 1000,
  sig_cutoff = 0.05,
  min_diff = 0.5
)
```

## Arguments

- mod_db:

  A DuckDB database connection object or path to the database.

- table_name:

  Character. Name of the output table to store collapsed windows
  (default: "collapsed_windows").

- max_distance:

  Numeric. The maximum allowable distance between consecutive
  significant windows for merging (default: 1000).

- sig_cutoff:

  Numeric. The significance threshold for adjusted p-values (default:
  0.05).

- min_diff:

  Numeric. The minimum absolute methylation difference required for
  inclusion in the analysis (default: 0.5).

## Value

This function does not return an object; it creates or replaces the
\`collapsed_windows\` table in the database.

## Details

The function performs the following steps:

- Filters the \`mod_diff_windows\` to retain only significant windows
  where \`p_adjust \<= sig_cutoff\` and \`ABS(meth_diff) \>= min_diff\`.

- Assigns a new region identifier based on proximity (\`max_distance\`)
  and the direction of methylation differences.

- Collapses regions by grouping contiguous windows, computing the
  average methylation difference (\`avg_meth_diff\`), and counting the
  number of merged windows.
