# Summarize per-position methylation calls into a DuckDB table

Aggregates row-level mod calls (from `input_table`, typically `"calls"`)
into a per-sample, per-genomic-position table (`output_table`, default
`"positions"`). Counts and fractions are created for the unmodified
class and for any set of user-defined modification codes and/or
combinations (e.g., `"m"`, `"h"`, `"m + h"`, `"a"`).

## Usage

``` r
summarize_mod_positions(
  mod_db,
  input_table = "calls",
  output_table = "positions",
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

  A path to a `.mod.db` DuckDB file or a `"mod_db"` object; a connection
  is opened via internal helpers and closed on return.

- input_table:

  Name of the source table containing call-level records (default
  `"calls"`). Must include at least: `sample_name`, `chrom`, `start`,
  `call_code`.

- output_table:

  Name of the destination positions table to create/extend (default
  `"positions"`).

- chrs:

  Character vector of chromosome/name filters. Rows whose `chrom` match
  any value are retained. Defaults to common human aliases (1–22,
  `chrX`, `chrY`, `chrM`, etc.).

- samples:

  Optional character vector of `sample_name`s to include. If `NULL`, all
  samples present in `input_table` are processed.

- mod_code:

  Character vector of modification specifications to count. Each entry
  is either a single code (e.g., `"m"`, `"h"`, `"a"`) or a
  `"code1 + code2"` combination. Default: `c("m","h","m + h")`.

- unmod_code:

  The call code representing unmodified calls (default `"-"`).

- unmod_label:

  Label used to name unmodified columns (default `"c"` yielding
  `c_counts` and `c_frac`).

- min_num_calls:

  Minimum total calls required at a position to be written (default
  `1`).

- temp_dir:

  Directory for DuckDB temporary files (default
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html)).

- threads:

  Integer DuckDB thread count. If `NULL`, uses an internal heuristic
  (typically all-but-one core).

- memory_limit:

  DuckDB memory limit string (e.g., `"16384MB"`). If `NULL`, an internal
  heuristic (~80% of RAM) is used.

- overwrite:

  If `TRUE` and `output_table` exists, it is dropped before writing.

## Value

(Invisibly) a `"mod_db"` object pointing to the same DB file with
`current_table` set to `output_table`. The created table has columns:

- `sample_name`, `chrom`, `start`, `end`, `num_calls`,

- for each label in `c(unmod_label, parsed(mod_code))`:
  `<label>_counts`, `<label>_frac`.

## Details

The function:

1.  Opens a DB connection and configures DuckDB pragmas
    (`temp_directory`, `threads`, `memory_limit`).

2.  Determines the list of samples to process (all by default, or the
    intersection with `samples`).

3.  Builds dynamic SQL to compute, per sample and position (`chrom`,
    `start`): `num_calls`, `<label>_counts` for each requested
    code/combination, and `<label>_frac` = `<label>_counts / num_calls`.

4.  Pre-creates the `output_table` schema with the appropriate dynamic
    columns, then inserts rows per sample. `end` is set equal to
    `start`.

Positions with `num_calls < min_num_calls` are skipped. Chromosome
filtering is done in SQL.

## How modification codes work

Pass `mod_code` as character values (single codes) or `"code1 + code2"`
to build combinations. Column labels are derived by removing spaces and
`"+"`. For example:

- `mod_code = c("m", "h", "m + h")` produces columns `m_counts`,
  `h_counts`, `mh_counts`

- `mod_code = "a"` produces `a_counts`

The unmodified class is controlled by `unmod_code` (e.g., `"-"`), with a
column name prefix set by `unmod_label` (default `"c"` for `c_counts`,
`c_frac`).

## See also

[`make_mod_db`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/make_mod_db.md),
[`summarize_mod_regions`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/summarize_mod_regions.md),
[`summarize_mod_windows`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/summarize_mod_windows.md),
[`calc_mod_diff`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/calc_mod_diff.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# From a calls table, build per-position summaries for default m/h classes:
summarize_mod_positions(
  mod_db       = "my_db.mod.db",
  input_table  = "calls",
  output_table = "positions"
)

# Restrict to specific samples and chromosomes, require at least 5 calls:
summarize_mod_positions(
  mod_db       = "my_db.mod.db",
  samples      = c("Cortical_Neurons","Astrocytes"),
  chrs         = c("chr1","chrX"),
  min_num_calls = 5
)

# Count a novel code "a" and an m+h combination; rename unmodified to 'u':
summarize_mod_positions(
  mod_db       = "my_db.mod.db",
  mod_code     = c("a", "m + h"),
  unmod_code   = "-",
  unmod_label  = "u"
)

# Recreate positions table (drop if exists):
summarize_mod_positions(
  mod_db       = "my_db.mod.db",
  overwrite    = TRUE
)
} # }
```
