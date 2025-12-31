# Create a Compressed Methylation Call Archive (.ch3) from a calls file.

Reads a delimited methylation call file and writes it to a compressed
archive format. Useful for standardizing and compressing methylation
data across multiple samples.

## Usage

``` r
make_mod_archive(file_name, sample_name, out_path, short_ids = TRUE)
```

## Arguments

- file_name:

  Path to the input file (tab-delimited) containing methylation calls.

- sample_name:

  Name of the sample to embed in the output archive filenames.

- out_path:

  Output directory where the \`.ch3\` files will be written.

- short_ids:

  Logical; default is \`TRUE\`, shortens \`read_id\` by trimming
  prefixes to reduce file size.

## Value

(Invisibly) the path template to the output archive files. Also prints a
message with timing information.

## Details

The input file should contain specific columns such as \`read_id\`,
\`chrom\`, \`ref_position\`, \`ref_mod_strand\`, \`query_kmer\`,
\`call_prob\`, \`call_code\`, \`base_qual\`, and \`flag\`. The function
filters out unplaced '-' strand reads with position 0, adds new fields
like \`start\` and \`end\`, and writes the result as a compressed Arrow
dataset with \`zstd\` compression.

## Examples

``` r
if (FALSE) { # \dontrun{
make_mod_archive("calls.tsv", "sample1", "output/", short_ids = TRUE)
} # }
```
