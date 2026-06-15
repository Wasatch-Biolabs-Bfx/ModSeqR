# ModSeqR v1.3.0 Release Notes

---

## New statistical methods (calc_mod_diff)

- **`calc_type = "welch_t"`** — Welch's (unequal-variance) t-test on per-sample
  modification fractions, computed entirely in DuckDB. Per-sample (no
  pseudoreplication); the t statistic is mapped to a normal deviate via the
  Wallace approximation. Validated against `t.test` (cor 1.000, max |Δp| ~3e-4).
- **`calc_type = "prop_z"`** — pooled two-proportion z / chi-square test, fully
  in DuckDB. Fast; pooled, so it shares Fisher's read-as-observation caveat.
  Matches `chisq.test(correct = FALSE)` to ~1e-5.
- **`calc_type = "quasi_bin"`** — closed-form, fully-in-DuckDB overdispersion-
  corrected proportion test (quasi-likelihood / Rao-Scott): a pooled-count z
  deflated by sqrt(Pearson dispersion, floored at 1). Approximates the exact
  `beta_bin` LRT without per-locus optimisation (cor ~0.99 with `beta_bin` on
  test data); use it as a fast screen and `beta_bin` for final small-sample
  inference. Reports the estimated `overdispersion` per locus.

## Parallelism

- **`calc_mod_diff(n_cores = )`** parallelises the R-backed tests (`fast_fisher`,
  `r_fisher`, `beta_bin`, `log_reg`) across cores. Because this DuckDB build
  locks a `.mod.db` exclusively even for read-only access, the parent exports the
  labelled input to a chromosome-partitioned Parquet dataset (one scan) and each
  PSOCK worker reads one chromosome's partition through its own in-memory DuckDB,
  tests it, and returns the compact result. Output is identical to the serial
  path (verified bit-for-bit). Default `n_cores = 1` (serial). The in-DuckDB
  tests (`wilcox`, `welch_t`, `prop_z`, `quasi_bin`) ignore `n_cores`.

---

# ModSeqR v1.2.3 Release Notes

---

## Bug fixes

- **BH correction no longer mislabels NULL p-values as maximally significant.**
  The in-DuckDB BH step wrapped p-values in `GREATEST(p_val, dbl_min)`, but DuckDB's
  `GREATEST` skips NULLs, so any locus with a NULL p-value (e.g. a degenerate
  rank-sum where a group has no covered samples) was coerced to `dbl_min`
  (2.2e-308) — i.e. flagged the *most* significant. NULL p-values are now preserved
  through ranking, the BH denominator (`COUNT(p_val)`), and the output: such loci
  keep NULL for both `p_val` and `p_adjust`.

## New features

- **`calc_mod_diff(min_samples=)` coverage filter.** Drops loci with fewer than
  `min_samples` covered samples in either group *before* BH correction, so
  under-powered loci don't inflate the multiple-testing denominator. Loci where a
  group has zero covered samples are now always removed (the test is undefined
  there), independent of `min_samples`.

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
