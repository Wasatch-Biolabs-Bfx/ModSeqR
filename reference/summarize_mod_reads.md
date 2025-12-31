# Summarize Reads in a Database

This function summarizes reads from a database, filtering and processing
the data based on a provided key table (if given). It computes
statistics on the reads such as the number of calls, CpG positions, and
fractions of methylation (\`m\`), hemi-methylation (\`h\`), and total
calls. The function interacts with the database to generate a \`reads\`
table.

## Usage

``` r
summarize_mod_reads(
  mod_db,
  input_calls_table = "calls",
  output_reads_table = "reads",
  regions_table = NULL,
  min_length = 100,
  min_CGs = 5
)
```

## Arguments

- mod_db:

  A character string specifying the path to the DuckDB database.

- min_length:

  An integer specifying the the minimum read_length.

- min_CGs:

  An integer specifying the minimum number of CG sites required for a
  read to be included in the summary.

- table_name:

  A string specifying what the user would like the name to be called in
  the database. Default is "reads".

## Value

Invisibly returns the database object. The function also outputs a
success message and the first few rows of the summarized \`reads\`
table.

## Details

The function connects to the provided DuckDB database, optionally
filters reads based on the key table, and then summarizes the read data.
It creates a temporary table for the filtered reads (if a key table is
provided) and creates a summary table called \`reads\` with information
on the total number of calls, the positions of the first and last CG
sites, and counts for different types of calls (\`m\`, \`h\`, and
\`-\`).

## Examples

``` r
#Specify the path to the database
 mod_db <- system.file("my_data.mod.db", package = "ModSeqR")
 region_bed = system.file("Islands_hg38_test.csv", package = "ModSeqR")
 
 # Summarize Reads
 summarize_mod_reads(mod_db, region_bed)
#> Summarizing Reads...
#> Error in dbSendQuery(conn, statement, ...): Parser Error: syntax error at or near "/"
#> 
#> LINE 17: FROM /home/runner/work/_temp/Library/ModSeqR/Islands_hg38_test...
#>               ^
#> 
#> LINE 17: FROM /home/runner/work/_temp/Library/ModSeqR/Islands_hg38_test...
#>               ^
#> ℹ Context: rapi_prepare
#> ℹ Error type: PARSER
```
