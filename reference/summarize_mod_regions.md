# Summarize methylation by annotated regions

Aggregates call-level rows from `input_table` (typically `"calls"`) into
per-sample region summaries written to `output_table` (default
`"regions"`). Regions are provided via a BED/TSV/CSV file with columns
`chrom`, `start`, `end`, and optional `region_name`. For each region the
function computes: number of CpG positions (rows), total calls,
per-class counts, and per-class fractions.

## Usage

``` r
summarize_mod_regions(
  mod_db,
  input_table = "calls",
  output_table = "regions",
  region_file,
  join = c("inner", "left", "right"),
  chrs = c(as.character(1:22), paste0("chr", 1:22), "chrX", "chrY", "chrM", paste0("Chr",
    1:22), "ChrX", "ChrY", "ChrM"),
  samples = NULL,
  mod_code = c("m", "h", "m + h"),
  unmod_code = "-",
  unmod_label = "c",
  min_num_calls = 1,
  temp_dir = tempdir(),
  threads = NULL,
  memory_limit = NULL,
  overwrite = TRUE
)
```

## Arguments

- mod_db:

  Path to a `.mod.db` DuckDB file or a `"mod_db"` object. A connection
  is opened via internal helpers and cleaned on return.

- input_table:

  Source table containing call-level records (default `"calls"`). Must
  contain at least: `sample_name`, `chrom`, `start`, `end`, `call_code`.

- output_table:

  Destination table name (default `"regions"`).

- region_file:

  BED/TSV/CSV path with columns `chrom`, `start`, `end` and optional
  `region_name`. If missing, `region_name` is synthesized as
  `"chrom_start_end"`.

- join:

  Join type between positions and regions: one of `"inner"`, `"left"`,
  or `"right"` (default `"inner"`).

- chrs:

  Character vector of chromosome filters; rows whose `chrom` match any
  value are retained. Defaults to common human aliases (1–22, `chrX`,
  `chrY`, `chrM`, …).

- samples:

  Optional character vector of `sample_name`s to include. If `NULL`, all
  samples present in `input_table` are processed.

- mod_code:

  Character vector of modification specs to count (single codes or
  `"code1 + code2"` combinations). Default `c("m","h","m + h")`.

- unmod_code:

  Call code representing unmodified (default `"-"`).

- unmod_label:

  Label used to name unmodified columns (default `"c"`).

- min_num_calls:

  Minimum total calls required at the *region* level to be written
  (default `1`). Regions below this threshold are skipped.

- temp_dir:

  Directory for DuckDB temporary files (default
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html)).

- threads:

  Integer DuckDB thread count. If `NULL`, an internal heuristic
  (typically all-but-one core) is used.

- memory_limit:

  DuckDB memory limit string (e.g. `"16384MB"`). If `NULL`, an internal
  heuristic (~80% of RAM) is used.

- overwrite:

  If `TRUE` and `output_table` exists, it is dropped before writing.

## Value

(Invisibly) a `"mod_db"` object pointing to the same DB file with
`current_table` set to `output_table`. The created table has columns:

- `sample_name`, `region_name`, `chrom`, `start`, `end`, `num_CpGs`,
  `num_calls`,

- for each label in `c(unmod_label, parsed(mod_code))`:
  `<label>_counts`, `<label>_frac`.

## Details

The function:

1.  Reads `region_file` (CSV/TSV/BED) and normalizes columns. If
    `region_name` is absent, it is synthesized. Basic chromosome-prefix
    harmonization is performed when DB positions and annotation disagree
    on presence of a `"chr"` prefix.

2.  Configures DuckDB pragmas (`temp_directory`, `threads`,
    `memory_limit`).

3.  Builds per-position counts (one row per `sample_name`, `chrom`,
    `start`) for the requested classes (`<label>_counts`, plus fractions
    later).

4.  Joins positions to regions using the chosen `join` type and
    aggregates per region: `num_CpGs`, `num_calls`, `<label>_counts`,
    and `<label>_frac` = `<label>_counts / num_calls`.

## How modification codes work

Pass `mod_code` as single codes (e.g. `"m"`, `"h"`, `"a"`) or
combinations with `"+"` (e.g. `"m + h"`). Labels are created by removing
spaces and `"+"` (e.g. `"m + h"` → `"mh"`). For each label the table
includes `<label>_counts` and `<label>_frac`. The unmodified class is
defined by `unmod_code` (default `"-"`), named using `unmod_label`
(default `"c"` → `c_counts`, `c_frac`).

## See also

[`make_mod_db`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/make_mod_db.md),
[`summarize_mod_positions`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/summarize_mod_positions.md),
[`summarize_mod_windows`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/summarize_mod_windows.md),
[`calc_mod_diff`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/calc_mod_diff.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Default m/h summary by regions
summarize_mod_regions(
  mod_db      = "my_db.mod.db",
  region_file = "islands_hg38.bed"
)

# Novel 'a' code and m+h combination, left join to keep empty regions
summarize_mod_regions(
  mod_db      = "my_db.mod.db",
  region_file = "islands_hg38.csv",
  mod_code    = c("a","m + h"),
  join        = "left",
  min_num_calls = 10
)
} # }
```
