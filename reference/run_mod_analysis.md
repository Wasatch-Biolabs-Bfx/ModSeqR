# Run Differential Analysis on Methylation Data

This function performs a differential analysis on methylation data based
on the specified call type and applies the appropriate summarization
method. It supports analysis of positions, regions, or windows. The
function handles the summarization of methylation data and performs
differential modification analysis based on case-control comparisons.

## Usage

``` r
run_mod_analysis(
  mod_db,
  out_path,
  call_type,
  region_file = NULL,
  window_size = 1000,
  step_size = 10,
  cases,
  controls,
  mod_type = "mh",
  calc_type = "fast_fisher",
  p_val_max = 0.05
)
```

## Arguments

- mod_db:

  A \`mod_db\` object representing the DuckDB database containing
  methylation data. The database should include necessary tables for the
  analysis, such as positions, regions, or windows.

- out_path:

  The directory in which the "Mod_Diff_Analysis_Results" directory
  containing result data will be written out too. If the user does not
  provide a directory, the working directory will be used.

- call_type:

  A character string specifying the type of data to analyze. Must be one
  of: `"positions"`, `"regions"`, or `"windows"`. This determines the
  summarization approach to use.

- region_file:

  A character string specifying the path to the region annotation file
  (required if \`call_type\` is \`"regions"\`). This file should be in a
  supported format (e.g., BED, CSV, TSV).

- window_size:

  An integer specifying the window size for summarizing methylation data
  if \`call_type\` is \`"windows"\`. The default value is 1000.

- step_size:

  An integer specifying the step size for sliding windows if
  \`call_type\` is \`"windows"\`. The default value is 10.

- cases:

  A character vector of sample names to be used as cases in the
  differential analysis. This argument is required and cannot be NULL.

- controls:

  A character vector of sample names to be used as controls in the
  differential analysis. This argument is required and cannot be NULL.

- mod_type:

  A character string specifying the modification type to analyze. The
  default is \`"mh"\`, which includes both methylation and
  hydroxymethylation. Other options are \`"c"\` for unmodified cytosine,
  \`"m"\` for methylation, and \`"h"\` for hydroxymethylation.

- calc_type:

  A character string specifying the type of statistical test to use for
  the differential analysis. The default is \`"fast_fisher"\`, but other
  calculation methods can be implemented.

- p_val_max:

  The p value threshold in which significant differentially modified
  positions/regions/windows will be written out in the final directory.

## Value

The result of the differential analysis, typically in the form of a
table or data frame with calculated statistics and p-values for each
position, region, or window, depending on the \`call_type\`. The result
is printed to the console.

## Details

This function first summarizes the methylation data by the specified
call type (positions, regions, or windows). It then proceeds with a
differential modification analysis between the provided case and control
samples. The analysis is tailored based on the selected modification
type (\`mod_type\`) and calculation method (\`calc_type\`).
