# Create a Modifications DuckDB from Parquet CH3 files (with optional sample naming)

Build a DuckDB database containing filtered modification call data from
one or more `.ch3` parquet files. Inputs may be individual files,
directories (expanded to all `*.ch3` files), or a \*\*named character
vector\*\* where names are used as `sample_name`s in the output.

## Usage

``` r
make_mod_db(
  ch3_files,
  db_name,
  chrom = NULL,
  min_read_length = 50,
  min_call_prob = 0.9,
  min_base_qual = 10,
  flag = NULL
)
```

## Arguments

- ch3_files:

  Character vector of CH3 parquet file paths and/or directories. May be
  a *named* vector to assign `sample_name`s explicitly; any entry of the
  form `NAME=PATH` is also accepted. Directories are scanned
  (non-recursively) for `*.ch3` files. Must not be empty.

- db_name:

  Path (without or with `.mod.db` extension) for the DuckDB database to
  be created; `.mod.db` is appended if missing.

- chrom:

  Optional chromosome filter. Either a single string (e.g., `"chr1"`) or
  a character vector (e.g., `c("chr1","chr2","chrX")`). If `NULL`, all
  chromosomes are included.

- min_read_length:

  Minimum read length to keep (default `50`).

- min_call_prob:

  Minimum call probability to keep (default `0.9`).

- min_base_qual:

  Minimum base quality to keep (default `10`).

- flag:

  Optional numeric flag value to require; if `NULL`, no flag filter.

## Value

(Invisibly) a list of class `"mod_db"` with elements:

- `db_file`: path to the created DuckDB file,

- `current_table`: `NULL` (set by downstream functions),

- `con`: connection is closed by cleanup and set to `"none"`.

The database contains at least the `calls` table.

## Details

**What it does**

- Expands `ch3_files` (handling directories and named entries) into a
  mapping of source files and optional `sample_name`s.

- Configures DuckDB pragmas for temp directory, thread count
  (all-but-one core), and a memory limit (~50

- Drops any existing tables in the target DB.

- Reads all input `.ch3` parquet files in a single pass and creates a
  table `calls` with columns: `sample_name`, `chrom`, `start`, `end`,
  `read_position`, `call_code`, `read_length`, `call_prob`, `base_qual`,
  `flag`. When names are not given for inputs, `sample_name` defaults to
  the file stem.

- Applies pushdown filters based on `chrom`, `min_read_length`,
  `min_call_prob`, `min_base_qual`, and `flag`.

**Side effects and performance**

- Creates (or overwrites) a DuckDB file at `db_name`.

- Uses a temp directory for DuckDB spills under
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- A temporary in-memory table `file_map` may be created for input
  mapping.

## Input forms

- **Files**: `c("a.ch3", "b.ch3")`

- **Directories**: `c("dir_of_ch3s/")` (expands to all `*.ch3`)

- **Named files/dirs**: `c(SampleA = "a.ch3", SampleB = "dir/")` — names
  become `sample_name`. If a name is not provided, the filename stem
  (without `.ch3`) is used.

## See also

[`summarize_mod_positions`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/summarize_mod_positions.md),
[`summarize_mod_regions`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/summarize_mod_regions.md),
[`summarize_mod_windows`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/summarize_mod_windows.md),
[`get_mod_dbinfo`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/get_mod_dbinfo.md),
[`get_mod_tableinfo`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/get_mod_tableinfo.md),
[`calc_mod_diff`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/calc_mod_diff.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# 1) Directory of CH3 files (non-recursive scan for *.ch3)
make_mod_db(ch3_files = "path/to/ch3_dir",
            db_name   = "my_db")

# 2) Explicit files (auto-sample names from stems)
make_mod_db(ch3_files = c("A.ch3", "B.ch3"),
            db_name   = "two_samples.mod.db",
            min_read_length = 100, min_base_qual = 10)

# 3) Named inputs (sample_name set from names)
make_mod_db(
  ch3_files = c(
    Sample1      = "../CH3/Sample1.ch3",
    Sample2  = "../CH3/Sample2.ch3"
  ),
  db_name = "My_DB",
  min_base_qual = 10,
  min_read_length = 100
)

# 4) Filter to specific chromosomes
make_mod_db(
  ch3_files = c(S1 = "A.mod", S2 = "B.mod"),
  db_name   = "chr1_chrX_only",
  chrom     = c("chr1","chrX")
)
} # }
```
