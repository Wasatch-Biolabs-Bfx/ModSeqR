## Empirical-Bayes beta-binomial differential test (calc_type = "eb_beta_bin").
##
## Ports the method used by FerruMod's ModDiff `calc_mod_diff_eb`: estimate each
## locus's beta-binomial dispersion by method-of-moments, SHRINK it toward a
## genome-wide prior (empirical Bayes), then run a beta-binomial GLM
## likelihood-ratio test with the dispersion HELD FIXED at the shrunk value, so
## only the mean coefficients (intercept + group + optional covariates) are
## optimised. Fixing the shrunk dispersion stabilises small-sample inference --
## the failure mode of both `quasi_bin` (noisy phi floored at 1, anti-conservative)
## and the exact `beta_bin` (noisy per-locus phi estimated jointly with the means).
##
## Design split (how far this stays in SQL):
##   * Pass 1 -- the entire EB-dispersion layer (per-locus MoM rho, the genome-wide
##     median prior, and the shrinkage) runs fully in DuckDB SQL (`.eb_shrunk_dispersion`).
##   * Pass 2 -- the per-locus GLM LRT (`.calc_diff_eb_betabin`, in calc_diff_betabin.R)
##     runs in R, because fitting logit(mu_i) = X beta per locus needs iterative
##     optimisation that SQL cannot express. It reuses the per-chromosome streaming /
##     parquet-parallel machinery via the dedicated `.calc_diff_eb_run` driver below.


## --- Pass 1: shrunk dispersion, fully in DuckDB ----------------------------
##
## Given the labelled per-sample lazy `in_dat` (sample_name, group_vars,
## num_calls, mod_counts, exp_group, plus any covariate columns), compute a
## per-locus shrunk dispersion `_rho_shrunk` and return `in_dat` augmented with
## that column (broadcast per locus), so it rides through both the serial and
## parquet-export parallel paths unchanged.
##
## Per locus, pooling all samples (single shared dispersion, as beta_bin assumes):
##   p_bar  = SUM(mod_counts) / SUM(num_calls)         (pooled fraction)
##   s2     = VAR_SAMP(per-sample fraction)            (between-sample variance)
##   n_bar  = AVG(num_calls)                           (mean depth)
##   v_b    = p_bar (1 - p_bar) / n_bar                (binomial expectation)
##   rho^   = (s2 - v_b) / (p_bar (1 - p_bar) - v_b),  clamped to 0 when s2 <= v_b
## Prior:   rho_prior = median(rho^) over loci with >= 2 samples.
## Shrink:  rho~ = (rho^ * s_count + rho_prior * df_prior) / (s_count + df_prior).
##
## The per-locus table is materialised to a temp table on `con`; the returned
## lazy tbl LEFT JOINs it back onto the rendered input. `disp_table` names the
## temp table so the caller can drop it.
.eb_shrunk_dispersion <- function(in_dat, group_vars, df_prior = 10,
                                  disp_table = "_eb_disp")
{
  con       <- dbplyr::remote_con(in_dat)
  qi        <- .qi_fun(con)
  src_sql   <- as.character(dbplyr::sql_render(in_dat))
  gvars_sql <- qi(group_vars)
  df_prior  <- as.numeric(df_prior)

  # Per-locus MoM dispersion + genome-wide median prior + shrinkage, all in SQL.
  disp_sql <- glue::glue("
    WITH
    _frac AS (
      SELECT *, mod_counts * 1.0 / num_calls AS _mf
      FROM ({src_sql}) _s
      WHERE num_calls > 0 AND mod_counts IS NOT NULL
    ),
    _perloc AS (
      SELECT
        {gvars_sql},
        COUNT(*)                              AS _s_count,
        SUM(mod_counts) * 1.0 / NULLIF(SUM(num_calls), 0) AS _pbar,
        VAR_SAMP(_mf)                         AS _s2,
        AVG(num_calls * 1.0)                  AS _nbar
      FROM _frac
      GROUP BY {gvars_sql}
    ),
    _disp AS (
      SELECT *,
        _pbar * (1.0 - _pbar) / NULLIF(_nbar, 0) AS _vb
      FROM _perloc
    ),
    _rho AS (
      SELECT *,
        CASE
          WHEN _pbar IS NULL OR _pbar <= 0 OR _pbar >= 1 OR _s2 IS NULL THEN 0.0
          WHEN _s2 > _vb AND (_pbar * (1.0 - _pbar) - _vb) > 0
            THEN (_s2 - _vb) / (_pbar * (1.0 - _pbar) - _vb)
          ELSE 0.0
        END AS raw_rho
      FROM _disp
    ),
    _med AS (
      SELECT MEDIAN(raw_rho) AS rho_prior
      FROM _rho
      WHERE _s_count >= 2
    )
    SELECT
      {gvars_sql},
      r._s_count AS num_samples_total,
      r.raw_rho,
      m.rho_prior,
      (r.raw_rho * r._s_count + m.rho_prior * {df_prior})
        / (r._s_count + {df_prior}) AS _rho_shrunk
    FROM _rho r CROSS JOIN _med m
  ")

  DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s",
                              DBI::dbQuoteIdentifier(con, disp_table)))

  # The dispersion table is a keyed lookup, so output row order is irrelevant.
  # Disabling insertion-order preservation lets DuckDB's hash aggregate over the
  # full (hundreds-of-millions-row) input spill freely instead of buffering to
  # hold order, which otherwise blows past memory_limit on large genomes. Restore
  # the previous setting once the table is materialised.
  prev_pio <- tryCatch(
    DBI::dbGetQuery(con, "SELECT current_setting('preserve_insertion_order') AS v")$v,
    error = function(e) NA_character_)
  DBI::dbExecute(con, "SET preserve_insertion_order=false")
  on.exit(if (!is.na(prev_pio))
    try(DBI::dbExecute(con, sprintf("SET preserve_insertion_order=%s", prev_pio)),
        silent = TRUE), add = TRUE)

  DBI::dbExecute(con, sprintf("CREATE TEMP TABLE %s AS %s",
                              DBI::dbQuoteIdentifier(con, disp_table), disp_sql))

  # Return the input augmented with the per-locus shrunk dispersion.
  joined_sql <- glue::glue("
    SELECT _s.*, _d._rho_shrunk
    FROM ({src_sql}) _s
    LEFT JOIN {DBI::dbQuoteIdentifier(con, disp_table)} _d USING ({gvars_sql})
  ")
  dplyr::tbl(con, dplyr::sql(joined_sql))
}


## --- Pass 2 driver: per-locus fixed-rho GLM LRT, streamed by chromosome -----
##
## Mirrors .calc_diff_stream_by_chrom / .calc_diff_stream_parallel but for the EB
## path: `in_dat` already carries covariate columns and `_rho_shrunk`, so those
## must be excluded from the locus `group_vars`. A dedicated driver keeps the
## existing fast_fisher/beta_bin paths untouched.
.calc_diff_eb_run <- function(mod_db, in_dat, con, out_table, mod_type,
                              group_vars, covariate_cols, n_cores = 1L)
{
  reserved <- c("sample_name", "exp_group", "num_calls", "mod_counts",
                "num_sites", "_rho_shrunk", covariate_cols)
  group_vars <- setdiff(group_vars, reserved)

  use_parallel <- !is.null(n_cores) && n_cores > 1L && "chrom" %in% colnames(in_dat)

  if (use_parallel) {
    .calc_diff_eb_run_parallel(mod_db, in_dat, con, out_table, mod_type,
                               group_vars, covariate_cols, n_cores)
    return(invisible(out_table))
  }

  # Serial: one chromosome at a time (or a single pass if no chrom column).
  if ("chrom" %in% colnames(in_dat)) {
    chroms <- in_dat |> dplyr::distinct(chrom) |> dplyr::pull(chrom)
  } else {
    chroms <- NA_character_
  }

  wrote_any <- FALSE
  for (chr in chroms) {
    slice <- if (is.na(chr)) in_dat else dplyr::filter(in_dat, chrom == chr)
    res <- .calc_diff_eb_betabin(slice, group_vars, covariate_cols)
    if (is.null(res) || nrow(res) == 0) next
    res <- dplyr::rename_with(res, ~ gsub("^mod", mod_type, .x))
    DBI::dbWriteTable(con, out_table, as.data.frame(res), append = wrote_any)
    wrote_any <- TRUE
  }

  if (!wrote_any) {
    stop("calc_mod_diff(): no loci had calls in both groups (or all were ",
         "removed by filtering). Nothing was written to '", out_table, "'.")
  }
  invisible(out_table)
}


## Parallel variant: export the augmented input (with covariates + _rho_shrunk)
## to a chrom-partitioned Parquet dataset, then test one chromosome per worker
## via its own in-memory DuckDB. Mirrors .calc_diff_stream_parallel but exports
## the PASSED in_dat (not a rebuilt one) so the EB columns are preserved.
.calc_diff_eb_run_parallel <- function(mod_db, in_dat, con, out_table, mod_type,
                                       group_vars, covariate_cols, n_cores)
{
  base_tmp <- mod_db$config$temp_dir %||% file.path(tempdir(), "modseqr_duckdb_tmp")

  chroms <- sort(dplyr::pull(dplyr::distinct(in_dat, chrom), chrom))
  if (length(chroms) == 0) {
    stop("calc_mod_diff(): input has no rows to test.")
  }

  export_dir <- file.path(base_tmp,
                          sprintf("modseqr_eb_paralleldiff_%d_%d",
                                  Sys.getpid(), as.integer(Sys.time())))
  unlink(export_dir, recursive = TRUE)
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(export_dir, recursive = TRUE), add = TRUE)

  src_sql <- as.character(dbplyr::sql_render(in_dat))
  DBI::dbExecute(con, sprintf(
    "COPY (%s) TO '%s' (FORMAT parquet, PARTITION_BY (chrom), OVERWRITE_OR_IGNORE)",
    src_sql, export_dir))

  n_workers <- min(as.integer(n_cores), length(chroms))

  # CRITICAL: bound each worker's in-memory DuckDB. A default duckdb() grabs ~80%
  # of system RAM as its memory_limit, so N workers oversubscribe RAM and can OOM
  # the box. Split the parent's configured budget across workers (floor 2GB),
  # single-thread each, and point them at temp_dir so they spill instead of
  # ballooning.
  budget_gb <- suppressWarnings(as.numeric(
    sub("(?i)\\s*g(i?b)?\\s*$", "", mod_db$config$memory_limit %||% "", perl = TRUE)))
  if (is.na(budget_gb) || budget_gb <= 0) budget_gb <- 24
  worker_mem <- paste0(max(2L, as.integer(floor(budget_gb / n_workers))), "GB")

  cl <- parallel::makeCluster(n_workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterCall(cl, function(libs) {
    .libPaths(libs)
    suppressMessages(requireNamespace("ModSeqR", quietly = TRUE))
    invisible(NULL)
  }, .libPaths())

  results <- parallel::parLapply(cl, chroms, .calc_diff_eb_parallel_worker,
                                 export_dir = export_dir, mod_type = mod_type,
                                 group_vars = group_vars,
                                 covariate_cols = covariate_cols,
                                 worker_mem = worker_mem, temp_dir = base_tmp)

  wrote_any <- FALSE
  for (res in results) {
    if (is.null(res) || nrow(res) == 0) next
    DBI::dbWriteTable(con, out_table, res, append = wrote_any)
    wrote_any <- TRUE
  }
  if (!wrote_any) {
    stop("calc_mod_diff(): no loci had calls in both groups (or all were removed ",
         "by filtering). Nothing was written to '", out_table, "'.")
  }
  invisible(out_table)
}


## One PSOCK worker: read a single chromosome's Parquet partition via a private
## in-memory DuckDB (no shared-file lock), run the EB beta-binomial LRT, return
## the per-locus frame. Top-level so it serialises by namespace reference.
.calc_diff_eb_parallel_worker <- function(chrom, export_dir, mod_type,
                                          group_vars, covariate_cols,
                                          worker_mem = "4GB", temp_dir = tempdir())
{
  wcfg <- list(memory_limit = worker_mem, threads = "1", temp_directory = temp_dir)
  wcon <- DBI::dbConnect(duckdb::duckdb(config = wcfg))
  on.exit(tryCatch(DBI::dbDisconnect(wcon, shutdown = TRUE), error = function(e) NULL),
          add = TRUE)

  glob <- file.path(export_dir, "**", "*.parquet")
  q <- sprintf(
    "SELECT * FROM read_parquet('%s', hive_partitioning = true) WHERE chrom = '%s'",
    glob, gsub("'", "''", chrom))
  in_dat <- dplyr::tbl(wcon, dplyr::sql(q))

  res <- .calc_diff_eb_betabin(in_dat, group_vars, covariate_cols)
  if (is.null(res) || nrow(res) == 0) return(NULL)
  as.data.frame(dplyr::rename_with(res, ~ gsub("^mod", mod_type, .x)))
}
