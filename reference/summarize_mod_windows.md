# Summarize methylation in sliding windows

Aggregates call-level rows from `input_table` (typically `"calls"`) into
per-sample window summaries written to `output_table` (default
`"windows"`). Windows are created using a **tiling + offsets** strategy
so that every `start` position is assigned to one or more windows
depending on the `step_size`. For each window the function computes:
total CpG sites, total calls, per-class counts, and per-class fractions.

## Usage

``` r
summarize_mod_windows(
  mod_db,
  input_table = "calls",
  output_table = "windows",
  window_size = 1000,
  step_size = 10,
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
  is opened via internal helpers and closed/cleaned on return.

- input_table:

  Source table containing call-level records (default `"calls"`). Must
  contain at least: `sample_name`, `chrom`, `start`, `call_code`.

- output_table:

  Destination window table name (default `"windows"`).

- window_size:

  Integer window width in bases (default `1000`).

- step_size:

  Step, in bases, used to create staggered window offsets (default
  `10`). Offsets are `seq(1, window_size - 1, by = step_size)`.

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

  Minimum total calls required for a window to be written (default `1`).
  Windows below this threshold are skipped.

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

- `sample_name`, `chrom`, `start`, `end`, `num_CpGs`, `num_calls`,

- for each label in `c(unmod_label, parsed(mod_code))`:
  `<label>_counts`, `<label>_frac`.

## Details

For each sample, the function first aggregates per-position counts from
`input_table` (`num_calls` plus dynamically generated `<label>_counts`
per `mod_code`/`unmod_code`). It then creates sliding windows by
assigning each position to a window start computed as: \$\$temp\\start =
start - ((start - offset) \bmod window\\size).\$\$ For each `offset` in
`seq(1, window_size - 1, by = step_size)`, it sums counts over
`[temp_start, temp_start + window_size - 1]` and writes:

- `num_CpGs`: number of positions aggregated in the window

- `num_calls`: sum of `num_calls`

- `<label>_counts`: summed counts for each label

- `<label>_frac`: `<label>_counts / num_calls` (NULL if
  `num_calls == 0`)

Resource pragmas (`temp_directory`, `threads`, `memory_limit`) are set
via internal heuristics unless overridden.

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
[`summarize_mod_regions`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/summarize_mod_regions.md),
[`calc_mod_diff`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/calc_mod_diff.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Default m/h windows (1kb windows, 10bp staggered offsets)
summarize_mod_windows("my_db.mod.db")

# Custom mod codes with a novel 'a' code and stricter filtering
summarize_mod_windows(
  mod_db        = "my_db.mod.db",
  mod_code      = c("a", "m + h"),
  min_num_calls = 25,
  window_size   = 2000,
  step_size     = 20
)

# Limit to selected samples and chromosomes; recreate table if present
summarize_mod_windows(
  mod_db       = "my_db.mod.db",
  samples      = c("Astrocytes","Blood_Plasma"),
  chrs         = c("chr1","chrX"),
  overwrite    = TRUE
)
} # }
```
