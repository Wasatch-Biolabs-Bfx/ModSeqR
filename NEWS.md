# ModSeqR v1.2.2 Release Notes

---

## Performance & Memory

- **`wilcox` now streams per chromosome again** — the single full-dataset CTE
  (v1.2.1) ranked the entire genome-wide window table in one DuckDB query, which
  spilled tens of GB and could exhaust memory on large cohorts. `calc_mod_diff()`
  now routes `wilcox` through `.calc_diff_stream_by_chrom()` like the other R-backed
  methods: each chromosome's rank-sum is computed in DuckDB and its compact
  per-window result appended before the next chromosome is read, bounding peak
  memory to a single chromosome. Results are identical — ranking partitions by the
  locus (chrom, start, end), so no window spans chromosomes.

---

# ModSeqR v1.2.1 Release Notes

---

## Performance & Memory

- **DuckDB spill-to-disk now configured at connection time** — `memory_limit` (75% of RAM) and
  `temp_directory` are set via the `config=` parameter to `duckdb()` before any query runs,
  ensuring spill-to-disk is always active as a backstop for large datasets.

- **Per-chromosome loops removed from `fast_fisher` and `r_fisher`** — previously these methods
  iterated one chromosome at a time, accumulating 25+ DuckDB round-trips and one `CHECKPOINT` per
  chromosome. Replaced with a single SQL conditional aggregation over the full dataset; DuckDB
  aggregates into a compact pivoted frame (one row per locus) before anything enters R.

- **Per-chromosome loop removed from `wilcox`** — reverted to a single CTE over the full dataset,
  letting DuckDB parallelize across all chromosomes at once. Spill-to-disk handles datasets that
  exceed RAM within a single query.

---

# ModSeqR v1.2.0 Release Notes

---

## Bug Fixes

- **`log_reg`** — Fixed two pre-existing bugs: coverage was incorrectly computed as `num_calls + mod_counts` instead of `num_calls`, and a dead code block referenced a removed `ref_position` column. The likelihood-ratio test is also corrected to properly compare intercept-only vs. intercept+group models.
- **`get_mod_*` functions** — Now consistently return data objects rather than `mod_db` objects.

---

## Performance & Memory

Several functions could exhaust available RAM on large datasets due to missing DuckDB disk-spill configuration and the use of temporary tables that cannot be paged to disk. The following functions have been fixed:

`summarize_mod_positions` · `summarize_mod_windows` · `summarize_mod_regions` · `filter_mod_table` · `collapse_mod_windows` · `classify_mod_reads` · `calc_mod_diff`

Key changes:
- DuckDB temp directory, memory limit, and thread pragmas are now set consistently so spilling to disk works as intended
- All staging tables converted from `TEMP` to persistent (DuckDB can only page persistent tables to disk)
- Processing is chunked one chromosome at a time with a `CHECKPOINT` after each sample
- BH p-value adjustment and result sorting in `calc_mod_diff` are now performed entirely in DuckDB using window functions — previously these pulled the full result set into R

---

## New Features

- **`wilcox` fully in SQL** — The Wilcoxon rank-sum test now runs entirely inside DuckDB using window functions for midranks and tie correction. No locus-level data enters R memory regardless of dataset size. Note: uses the normal approximation with continuity correction (`exact=FALSE, correct=TRUE`), which is equivalent to R's behavior when ties are present.
- **`get_mod_result()`** — New getter function that retrieves the most recent computed result from a `mod_db` object, allowing results to be captured mid-pipe without breaking the chain.
- **`peek_mod_table()`** — New convenience function to preview a table without leaving the pipeline.
- **`get_mod_tablelist()`** — New convenience function to list all tables in the database.

---

## Other Changes

- **`beta_bin`** — Still requires R for MLE fitting (`stats::optim()`), but now fetches only the columns needed for the likelihood-ratio test and processes one chromosome at a time to cap peak memory usage. See documentation for guidance on when to prefer `fast_fisher` or `wilcox` on memory-constrained hardware.
- **Parameter naming** — Standardized to `input_table` / `output_table` across all pipeline functions. Previous parameter names are retained as deprecated aliases and will warn on use.
- **`last_result` slot** — All pipeline functions now populate `mod_db$last_result` with a preview of their output, accessible via `get_mod_result()`.
- **Maintainer** — Updated to Wasatch Biolabs.

---

## Tests

Replaced the stale `MethylSeqR`/`ch3_db` test suite with 53 new tests covering `calc_mod_diff` (all five calc types including SQL wilcox correctness), `filter_mod_table`, and `collapse_mod_windows`.
