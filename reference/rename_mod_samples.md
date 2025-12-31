# Rename sample names in a table

Update `sample_name` values in a specified DuckDB table using a mapping
from old names to new names. Accepts either a named character vector
(`c(old="new", ...)`), or a two-column data.frame with columns `old` and
`new`.

## Usage

``` r
rename_mod_samples(mod_db, table, samples_map, strict = TRUE, preview = TRUE)
```

## Arguments

- mod_db:

  Path to a `.mod.db` file or a `"mod_db"` object.

- table:

  Character scalar: the table to modify (e.g. `"positions"`,
  `"windows"`, `"regions"`, or `"calls"`).

- samples_map:

  Either a named character vector (`names = old, values = new`) or a
  data.frame with columns `old` and `new`.

- strict:

  Logical; if `TRUE` (default) stop on issues (missing old names,
  empty/NA new names, or duplicate mappings). If `FALSE`, tries to
  proceed after dropping invalid rows with a warning.

- preview:

  Logical; if `TRUE`, prints a small before/after summary of distinct
  sample names.

## Value

(Invisibly) the updated `"mod_db"` object (connection closed on return).

## Examples

``` r
if (FALSE) { # \dontrun{
# Named character vector
rename_mod_samples("my_db.mod.db", table = "positions",
                   samples_map = c("Astrocytes" = "Astro",
                                   "Cortical_Neurons" = "Cortical"))

# data.frame mapping
m <- data.frame(old = c("Ctrl1","Ctrl2"), new = c("Control_1","Control_2"))
rename_mod_samples("my_db.mod.db", "windows", m)
} # }
```
