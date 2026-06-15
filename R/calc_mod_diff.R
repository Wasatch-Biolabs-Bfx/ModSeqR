#' Calculate Differential Methylation
#'
#' This function calculates differential methylation between specified case and control groups using various statistical methods.
#' The results are stored in a DuckDB database for further analysis.
#'
#' @param mod_db A list containing the database file path. This should be a valid "mod_db" class object.
#' @param input_table A string representing the name of the table in the database from which to pull
#'   the data. Default is \code{"positions"}. Required columns: \code{sample_name}, \code{chrom},
#'   \code{start}, \code{end}.
#' @param output_table Destination table name for results. If NULL, defaults to \code{paste0("mod_diff_", input_table)}.
#' @param cases A character vector containing the sample names for the case group.
#' @param controls A character vector containing the sample names for the control group.
#' @param mod_type A string indicating the type of modification to analyze.
#' Default is "mh" for methylation/hydroxymethylation. Other codes include
#'   "a" for 6mA, "17596" for inosine, and "17802" for pseudouridine.
#'   Bare numeric codes are automatically prefixed with "m_".
#' @param calc_type A string specifying the statistical method to use.
#'   Options: \code{"wilcox"}, \code{"beta_bin"}, \code{"fast_fisher"}, \code{"r_fisher"},
#'   \code{"log_reg"}. Default is \code{NULL}, in which case:
#'   \itemize{
#'     \item \code{"wilcox"} if both groups have >= 5 samples
#'     \item \code{"fast_fisher"} if either group has fewer than 5 samples
#'   }
#'   All methods are processed \strong{one chromosome at a time}, streaming each
#'   chromosome's results to the output table before the next chromosome is read,
#'   so peak memory is bounded to a single chromosome rather than the whole genome.
#'   \code{"wilcox"} computes its rank-sum entirely in DuckDB and only collects
#'   the compact per-window result for each chromosome. \code{"fast_fisher"},
#'   \code{"r_fisher"}, \code{"log_reg"}, and \code{"beta_bin"} require R for their
#'   per-locus statistics.
#' @param min_sites Minimum number of distinct modification sites (e.g., CpGs)
#'   required per sample within a window. Windows where any sample has fewer
#'   than this many sites with calls are dropped before testing. This filters
#'   out windows with poor breadth of coverage. Only applies when the input
#'   table contains a \code{num_sites} column (i.e., windows).
#'   Default is \code{NULL} (no filtering).
#' @param min_samples Minimum number of covered samples required in \strong{each}
#'   group (case and control) at a locus for it to be tested. Loci below this are
#'   dropped \emph{before} BH correction so they do not inflate the multiple-testing
#'   denominator. Loci where either group has no covered samples are always removed
#'   (the test is undefined there) regardless of this setting. Default is
#'   \code{NULL}, which applies only the always-on empty-group removal (min 1 per group).
#' @param overwrite If TRUE and output_table exists, it is dropped before writing.
#' @param call_type Deprecated. Use \code{input_table} instead.
#'
#' @details
#' The function connects to the specified DuckDB database, retrieves methylation data from
#' \code{input_table}, and summarizes it for cases and controls. All calc types are
#' processed \strong{one chromosome at a time} (see \code{.calc_diff_stream_by_chrom}):
#' each chromosome is tested and its results streamed to the output table before the next
#' chromosome is read, so only one chromosome's results ever reside in R. \code{wilcox}
#' computes its rank-sum inside DuckDB per chromosome and collects only the compact
#' per-window result; \code{fast_fisher}, \code{r_fisher}, \code{log_reg}, and
#' \code{beta_bin} compute their per-locus statistics in R. BH p-value adjustment and final
#' sorting are performed entirely in DuckDB using window functions.
#'
#' \strong{Memory note:} per-chromosome streaming bounds peak memory to the largest
#' single chromosome. \code{wilcox} remains the cheapest option since its locus-level
#' ranking stays in DuckDB and only per-window summaries are collected.
#'
#' @return Invisibly returns the updated \code{"mod_db"} object with \code{current_table} set to
#'   the output table name and \code{last_result} set to a data frame preview (head) of the result
#'   table. Retrieve the preview with \code{get_mod_result(mod_db)}; retrieve the full table with
#'   \code{get_mod_table(mod_db, mod_db\$current_table)}.
#'
#' @examples
#' \dontrun{
#' # Specify the path to the database
#'  mod_db <- system.file("my_data.mod.db", package = "ModSeqR")
#'
#'  # Get methylation statistics for the 'positions' input table without plotting
#'  calc_mod_diff(mod_db = mod_db,
#'                input_table = "positions",
#'                cases = c("Blood1_chr21", "Blood2_chr21", "Blood3_chr21"),
#'                controls = c("Sperm1_chr21", "Sperm2_chr21", "Sperm3_chr21")))
#' }
#'
#' @importFrom DBI dbConnect dbDisconnect dbExistsTable dbRemoveTable dbExecute dbWriteTable dbGetQuery dbListFields
#' @importFrom duckdb duckdb
#' @importFrom dplyr tbl select any_of mutate case_when filter pull summarize inner_join join_by rename_with collect arrange distinct
#' @importFrom dbplyr sql_render remote_con
#' @importFrom glue glue
#' @importFrom stats fisher.test p.adjust dhyper phyper glm.fit pchisq optim plogis qlogis var
#'
#' @export

calc_mod_diff <- function(mod_db,
                          input_table = "positions",
                          output_table = NULL,
                          cases,
                          controls,
                          mod_type = "mh",
                          calc_type = NULL,
                          min_sites = NULL,
                          min_samples = NULL,
                          overwrite = TRUE,
                          call_type = NULL)
{
  start_time <- Sys.time()

  # Open the database connection
  mod_db <- .modhelper_connectDB(mod_db)

  # Handle deprecated call_type parameter
  if (!is.null(call_type)) {
    warning("'call_type' is deprecated; use 'input_table' instead.", call. = FALSE)
    input_table <- call_type
  }


  # check for input table
  if (!dbExistsTable(.get_con(mod_db), input_table)) {
    stop(input_table, " table does not exist. Build it with summarize_mod_positions(), ",
         "summarize_mod_regions(), or summarize_mod_windows().")
  }

  # Minimum column check
  .modhelper_check_cols(.get_con(mod_db), input_table, c("sample_name", "chrom", "start", "end"))

  # Discover available *_counts columns and validate mod_type
  cols <- colnames(dplyr::tbl(.get_con(mod_db), input_table))
  counts_cols <- grep("_counts$", cols, value = TRUE)
  available_labels <- sub("_counts$", "", counts_cols)

  mod_type <- mod_type[1]

  # Allow bare numeric codes: e.g. 17596 -> "m_17596"
  if (grepl("^\\d+$", mod_type)) {
    mod_type <- paste0("m_", mod_type)
  }

  mod_counts_col <- paste0(mod_type, "_counts")
  if (!mod_counts_col %in% cols) {
    stop("Requested mod_type '", mod_type, "' not found in ", input_table, ". ",
         "Available mod types are: ",
         paste(sort(available_labels), collapse = ", "), ".\n",
         "Tip: rebuild ", input_table, " with summarize_* and include mod_code = '", mod_type, "'.")
  }
  if (!"num_calls" %in% cols) {
    stop(input_table, " is missing required column 'num_calls'. ",
         "Recreate it with the latest summarize_* function.")
  }

  if (is.null(output_table) || !nzchar(output_table)) {
    mod_diff_table <- paste0("mod_diff_", input_table)
  } else {
    mod_diff_table <- output_table
  }

  if (DBI::dbExistsTable(.get_con(mod_db), mod_diff_table)) {
    if (overwrite) {
      DBI::dbRemoveTable(.get_con(mod_db), mod_diff_table)
    } else {
      stop("Output table '", mod_diff_table, "' already exists. Set overwrite = TRUE or choose a different output_table.")
    }
  }

  in_dat <-
    dplyr::tbl(.get_con(mod_db), input_table) |>
    dplyr::select(
      sample_name,
      dplyr::any_of(c("region_name", "chrom", "start", "end", "num_sites")),
      num_calls,
      mod_counts = dplyr::all_of(mod_counts_col)
    ) |>
    dplyr::mutate(
      exp_group = dplyr::case_when(
        sample_name %in% cases ~ "case",
        sample_name %in% controls ~ "control",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(exp_group))

  # Filter windows with insufficient site coverage
  if (!is.null(min_sites) && min_sites > 0) {
    if (!"num_sites" %in% cols) {
      warning("min_sites was set but '", input_table,
              "' has no 'num_sites' column. Filter skipped. ",
              "num_sites is only available for window/region tables.")
    } else {
      n_before <- in_dat |> dplyr::count() |> dplyr::pull(n)

      in_dat <- in_dat |>
        dplyr::filter(num_sites >= min_sites)

      n_after <- in_dat |> dplyr::count() |> dplyr::pull(n)

      message(
        "Site filter (min_sites = ", min_sites, "): ",
        format(n_after, big.mark = ","), " of ",
        format(n_before, big.mark = ","), " sample-windows passed ",
        "(dropped ", format(n_before - n_after, big.mark = ","), ")."
      )

      if (n_after == 0) {
        stop("All windows were removed by the min_sites filter. ",
             "Try a lower value (current: ", min_sites, ").")
      }

      remaining_samples <- in_dat |>
        dplyr::distinct(sample_name, exp_group) |>
        dplyr::collect()

      missing_cases <- cases[!cases %in% remaining_samples$sample_name]
      missing_ctrls <- controls[!controls %in% remaining_samples$sample_name]

      if (length(missing_cases) > 0) {
        warning("min_sites filter removed ALL windows for case sample(s): ",
                paste(missing_cases, collapse = ", "),
                ". Consider lowering min_sites.")
      }
      if (length(missing_ctrls) > 0) {
        warning("min_sites filter removed ALL windows for control sample(s): ",
                paste(missing_ctrls, collapse = ", "),
                ". Consider lowering min_sites.")
      }
    }
  }

  # Check sample names present
  all_samples <- unique(dplyr::pull(in_dat, sample_name))
  if (any(!cases %in% all_samples)) {
    missing <- paste(cases[!cases %in% all_samples], collapse = ", ")
    stop("Check case names - some case samples are missing from the data: ", missing)
  }
  if (any(!controls %in% all_samples)) {
    missing <- paste(controls[!controls %in% all_samples], collapse = ", ")
    stop("Check control names - some control samples are not in the data: ", missing)
  }

  # Auto-select calc_type if not provided --------------------------------------
  if (is.null(calc_type)) {
    n_case    <- length(cases)
    n_control <- length(controls)

    if (min(n_case, n_control) >= 5) {
      calc_type <- "wilcox"
    # } else if (min(n_case, n_control) >= 2) {
    #   calc_type <- "beta_bin"
    } else {
      calc_type <- "fast_fisher"
    }

    message(
      "Using '", calc_type, "' statistical method",
      " (cases = ", n_case, ", controls = ", n_control, ")..."
    )
  }

  message("Running differential analysis...\n")

  con <- .get_con(mod_db)

  # Compute p-values / diffs and populate `mod_diff_table` one chromosome at a
  # time via .calc_diff_stream_by_chrom(), which tests each chromosome and writes
  # its results to the table before reading the next, so peak memory is bounded
  # to a single chromosome rather than the whole genome.
  #   * wilcox computes its rank-sum entirely in DuckDB, but per chromosome: only
  #     the compact per-window result (one row per locus) is collected into R
  #     before being appended. Ranking is PARTITION BY the locus group_vars
  #     (chrom, start, end), so per-chromosome slicing is exactly equivalent to a
  #     genome-wide pass -- no window spans chromosomes.
  #   * fast_fisher / r_fisher / beta_bin / log_reg compute their per-locus
  #     statistic in R, also one chromosome at a time.
  per_chrom_fun <- switch(calc_type,
    wilcox      = function(slice, gv) dplyr::collect(.calc_diff_wilcox(slice)),
    fast_fisher = function(slice, gv) .calc_diff_fisher(slice, gv, calc_type = "fast_fisher"),
    r_fisher    = function(slice, gv) .calc_diff_fisher(slice, gv, calc_type = "r_fisher"),
    beta_bin    = function(slice, gv) .calc_diff_betabin(slice, gv),
    log_reg     = function(slice, gv) .calc_diff_logreg(slice, gv),
    stop("Unknown calc_type: ", calc_type,
         ". Use 'beta_bin', 'fast_fisher', 'r_fisher', 'wilcox', or 'log_reg'.")
  )
  .calc_diff_stream_by_chrom(in_dat, con, mod_diff_table, mod_type, per_chrom_fun)

  # Coverage filter: drop loci that cannot support a meaningful comparison --
  # either group has no covered samples (num_samples_* is NULL) or fewer than
  # min_samples covered samples. This is applied BEFORE the BH step so that
  # under-powered/degenerate loci do not inflate the multiple-testing denominator.
  # Loci with an empty group are always removed (a test there is undefined);
  # min_samples adds a stricter per-group minimum when supplied.
  if (DBI::dbExistsTable(con, mod_diff_table) &&
      all(c("num_samples_case", "num_samples_control") %in%
          DBI::dbListFields(con, mod_diff_table))) {
    floor_n <- if (!is.null(min_samples) && min_samples > 0) as.integer(min_samples) else 1L
    n_pre <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", mod_diff_table))$n
    DBI::dbExecute(con, sprintf(
      "DELETE FROM %s
       WHERE num_samples_case IS NULL OR num_samples_control IS NULL
          OR num_samples_case < %d OR num_samples_control < %d",
      mod_diff_table, floor_n, floor_n))
    n_post <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", mod_diff_table))$n
    if (n_post < n_pre) {
      message("Coverage filter (min_samples = ", floor_n, " per group): kept ",
              format(n_post, big.mark = ","), " of ", format(n_pre, big.mark = ","),
              " loci (dropped ", format(n_pre - n_post, big.mark = ","), ").")
    }
    if (n_post == 0) {
      stop("calc_mod_diff(): every locus was removed by the coverage filter ",
           "(min_samples = ", floor_n, "). Lower min_samples or check sample coverage.")
    }
  }

  # Compute BH-adjusted p-values entirely in DuckDB using window functions.
  # Avoids collecting the full result into R for p.adjust() + arrange().
  # Only non-NULL p-values are ranked and counted toward the BH denominator;
  # any locus with a NULL p-value (e.g. zero-variance rank-sum) keeps NULL for
  # both p_val and p_adjust rather than being coerced to a tiny value.
  dbl_min <- .Machine$double.xmin
  DBI::dbExecute(.get_con(mod_db), sprintf(
    "CREATE OR REPLACE TABLE %s AS
     WITH ranked AS (
       SELECT *,
         CASE WHEN p_val IS NOT NULL
              THEN ROW_NUMBER() OVER (ORDER BY p_val NULLS LAST) END AS _r,
         COUNT(p_val) OVER () AS _n
       FROM %s
     ),
     bh AS (
       SELECT *,
         CASE WHEN p_val IS NOT NULL
              THEN LEAST(p_val * CAST(_n AS DOUBLE) / _r, 1.0) END AS _bh_raw
       FROM ranked
     ),
     bh_cummin AS (
       SELECT *,
         MIN(_bh_raw) OVER (ORDER BY _r DESC NULLS LAST
                            ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS _p_adj
       FROM bh
     )
     SELECT * EXCLUDE (_r, _n, _bh_raw, _p_adj, p_val),
       CASE WHEN p_val IS NULL THEN NULL ELSE GREATEST(p_val,  %.17g) END AS p_val,
       CASE WHEN p_val IS NULL THEN NULL ELSE GREATEST(_p_adj, %.17g) END AS p_adjust
     FROM bh_cummin
     ORDER BY _p_adj NULLS LAST",
    mod_diff_table, mod_diff_table, dbl_min, dbl_min
  ))

  end_time <- Sys.time()
  total_time_difftime <- end_time - start_time

  # Convert the total_time_difftime object to numeric seconds for a reliable comparison
  total_seconds <- as.numeric(total_time_difftime, units = "secs")

  if (total_seconds > 60) {
    # If greater than 60 seconds, convert to numeric minutes for display
    total_minutes <- as.numeric(total_time_difftime, units = "mins")
    message("Mod diff analysis complete! ", mod_diff_table, " table successfully created!",
            "\nTime elapsed: ", round(total_minutes, 2), " minutes\n")
  } else {
    # Otherwise, display in numeric seconds
    message("Mod diff analysis complete! ", mod_diff_table, " table successfully created!",
            "\nTime elapsed: ", round(total_seconds, 2), " seconds\n")
  }

  # Detect table type to give informative message
  table_type <- .modhelper_detect_table_type(.get_con(mod_db), input_table)
  if (table_type == "windows") {
    message("Call collapse_mod_windows() to collapse significant windows.\n")
  }

  # Print a preview of what table looks like
  result_head <- dplyr::tbl(.get_con(mod_db), mod_diff_table) |> head() |> dplyr::collect()
  print(result_head)

  mod_db$current_table <- mod_diff_table
  mod_db$last_result   <- result_head
  mod_db <- .modhelper_cleanup(mod_db)
  invisible(mod_db)
}



## Stream an R-backed differential test one chromosome at a time.
##
## For calc types that must compute their per-locus statistic in R
## (fast_fisher, r_fisher, beta_bin, log_reg), this iterates over the
## chromosomes present in `in_dat`, hands each chromosome's lazy slice to
## `per_chrom_fun(slice, group_vars)` (which returns a locus-level data.frame),
## applies the mod_type column rename, and appends that chromosome's rows
## straight into `out_table` before moving on. Only one chromosome's data is
## ever materialized in R, and the full result never accumulates in memory.
##
## If `in_dat` has no `chrom` grouping column (it always does for positions /
## windows / regions tables), the whole table is processed as a single chunk.
.calc_diff_stream_by_chrom <- function(in_dat, con, out_table, mod_type, per_chrom_fun)
{
  group_vars <- setdiff(colnames(in_dat),
                        c("sample_name", "exp_group", "num_calls", "mod_counts", "num_sites"))

  if ("chrom" %in% group_vars) {
    chroms <- in_dat |> dplyr::distinct(chrom) |> dplyr::pull(chrom)
  } else {
    chroms <- NA_character_   # single pass over the whole table
  }

  wrote_any <- FALSE
  for (chr in chroms) {
    slice <- if (is.na(chr)) in_dat else dplyr::filter(in_dat, chrom == chr)

    res <- per_chrom_fun(slice, group_vars)
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


## Calculate p-values using Wilcoxon rank-sum test, fully in DuckDB SQL.
## Uses midranks (average of tied row numbers) for the U statistic, the standard
## tie-corrected variance formula, and the Abramowitz & Stegun 26.2.16 normal CDF
## approximation (max error ~7.5e-8). Always uses the normal approximation with
## continuity correction (wilcox.test(exact=FALSE, correct=TRUE)).
## Returns a tbl_lazy; connection-time spill-to-disk config handles datasets that
## exceed RAM within a single query.
.calc_diff_wilcox <- function(in_dat)
{
  con        <- dbplyr::remote_con(in_dat)
  group_vars <- setdiff(colnames(in_dat),
                        c("sample_name", "exp_group", "num_calls", "mod_counts", "num_sites"))

  qi <- function(nms) paste(
    vapply(nms, function(nm) as.character(DBI::dbQuoteIdentifier(con, nm)), character(1)),
    collapse = ", "
  )

  src_sql   <- as.character(dbplyr::sql_render(in_dat))
  gvars_sql <- qi(group_vars)

  sql <- glue::glue("
    WITH
    _frac AS (
      SELECT *, mod_counts * 1.0 / num_calls AS _mf
      FROM ({src_sql}) _s
      WHERE num_calls > 0 AND mod_counts IS NOT NULL
    ),
    _rn AS (
      SELECT *,
        ROW_NUMBER() OVER (PARTITION BY {gvars_sql} ORDER BY _mf) AS _row_n
      FROM _frac
    ),
    _ranked AS (
      SELECT *,
        AVG(CAST(_row_n AS DOUBLE)) OVER (PARTITION BY {gvars_sql}, _mf) AS _rank
      FROM _rn
    ),
    _grp AS (
      SELECT
        {gvars_sql}, exp_group,
        COUNT(*)        AS _n_g,
        SUM(_rank)      AS _rank_sum,
        SUM(num_calls)  AS _nc_g,
        SUM(mod_counts) AS _mc_g,
        AVG(_mf)        AS _mf_avg
      FROM _ranked
      GROUP BY {gvars_sql}, exp_group
    ),
    _tg AS (
      SELECT {gvars_sql}, _mf, COUNT(*) AS _t
      FROM _ranked
      GROUP BY {gvars_sql}, _mf
    ),
    _ties AS (
      SELECT {gvars_sql},
        SUM(POWER(CAST(_t AS DOUBLE), 3) - CAST(_t AS DOUBLE)) AS _tie_sum
      FROM _tg
      GROUP BY {gvars_sql}
    ),
    _locus AS (
      SELECT
        {gvars_sql},
        MAX(CASE WHEN exp_group = 'case'    THEN _n_g      END) AS num_samples_case,
        MAX(CASE WHEN exp_group = 'control' THEN _n_g      END) AS num_samples_control,
        MAX(CASE WHEN exp_group = 'case'    THEN _nc_g     END) AS num_calls_case,
        MAX(CASE WHEN exp_group = 'control' THEN _nc_g     END) AS num_calls_control,
        MAX(CASE WHEN exp_group = 'case'    THEN _mc_g     END) AS mod_counts_case,
        MAX(CASE WHEN exp_group = 'control' THEN _mc_g     END) AS mod_counts_control,
        MAX(CASE WHEN exp_group = 'case'    THEN _mf_avg   END) AS mod_frac_case,
        MAX(CASE WHEN exp_group = 'control' THEN _mf_avg   END) AS mod_frac_control,
        MAX(CASE WHEN exp_group = 'case'    THEN _rank_sum END) AS _W,
        MAX(CASE WHEN exp_group = 'case'    THEN _n_g      END) AS _n1,
        MAX(CASE WHEN exp_group = 'control' THEN _n_g      END) AS _n2
      FROM _grp
      GROUP BY {gvars_sql}
    ),
    _u AS (
      SELECT l.*,
        COALESCE(t._tie_sum, 0.0)      AS _tie_sum,
        _n1 * _n2 * 1.0 / 2.0         AS _mu_U,
        _W - _n1 * (_n1 + 1.0) / 2.0  AS _U,
        SQRT(
          _n1 * _n2 * 1.0 / 12.0 * (
            (_n1 + _n2 + 1.0) -
            COALESCE(t._tie_sum, 0.0) /
              NULLIF((_n1 + _n2) * (_n1 + _n2 - 1.0), 0.0)
          )
        ) AS _sig
      FROM _locus l
      LEFT JOIN _ties t USING ({gvars_sql})
    ),
    _z AS (
      SELECT *,
        CASE
          WHEN _sig IS NULL OR _sig = 0.0 THEN NULL
          WHEN _U > _mu_U THEN (_U - _mu_U - 0.5) / _sig
          WHEN _U < _mu_U THEN (_U - _mu_U + 0.5) / _sig
          ELSE 0.0
        END AS _z
      FROM _u
    ),
    _cdf AS (
      SELECT *,
        1.0 / (1.0 + 0.2316419 * ABS(_z))                        AS _t1,
        (1.0 / SQRT(2.0 * PI())) * EXP(-POWER(ABS(_z), 2) / 2.0) AS _phi
      FROM _z
    )
    SELECT
      {gvars_sql},
      num_samples_case,
      num_samples_control,
      num_calls_case,
      num_calls_control,
      mod_counts_case,
      mod_counts_control,
      mod_frac_case,
      mod_frac_control,
      mod_frac_case - mod_frac_control AS meth_diff,
      CASE WHEN _z IS NULL THEN NULL
      ELSE LEAST(2.0 * _phi * (
           0.319381530  * _t1
         - 0.356563782  * POWER(_t1, 2)
         + 1.781477937  * POWER(_t1, 3)
         - 1.821255978  * POWER(_t1, 4)
         + 1.330274429  * POWER(_t1, 5)
      ), 1.0) END AS p_val
    FROM _cdf
  ")

  dplyr::tbl(con, dplyr::sql(sql))
}


## Calculate p-values using Fisher exact test, for one chromosome's slice.
## Called once per chromosome by .calc_diff_stream_by_chrom(). Runs a single SQL
## conditional aggregation over the slice (one row per locus, already pivoted),
## collects that compact result into R, runs vectorized Fisher, and returns a
## data frame. Peak R memory is bounded to a single chromosome's loci.
.calc_diff_fisher <- function(in_dat, group_vars, calc_type)
{
  con <- dbplyr::remote_con(in_dat)

  qi <- function(nms) paste(
    vapply(nms, function(nm) as.character(DBI::dbQuoteIdentifier(con, nm)), character(1)),
    collapse = ", "
  )
  gvars_sql <- qi(group_vars)
  src_sql   <- as.character(dbplyr::sql_render(in_dat))

  agg_sql <- sprintf("
    SELECT %s,
      COUNT(DISTINCT CASE WHEN exp_group = 'case'    THEN sample_name END) AS num_samples_case,
      COUNT(DISTINCT CASE WHEN exp_group = 'control' THEN sample_name END) AS num_samples_control,
      SUM(CASE WHEN exp_group = 'case'    THEN num_calls                   ELSE 0 END) AS num_calls_case,
      SUM(CASE WHEN exp_group = 'control' THEN num_calls                   ELSE 0 END) AS num_calls_control,
      SUM(CASE WHEN exp_group = 'case'    THEN CAST(mod_counts AS DOUBLE)  ELSE 0 END) AS mod_counts_case,
      SUM(CASE WHEN exp_group = 'control' THEN CAST(mod_counts AS DOUBLE)  ELSE 0 END) AS mod_counts_control
    FROM (%s) _src
    GROUP BY %s
    HAVING num_calls_case > 0 AND num_calls_control > 0",
    gvars_sql, src_sql, gvars_sql
  )

  dat <- DBI::dbGetQuery(con, agg_sql)
  if (nrow(dat) == 0) return(data.frame())

  dat$c_counts_case    <- dat$num_calls_case    - dat$mod_counts_case
  dat$c_counts_control <- dat$num_calls_control - dat$mod_counts_control
  dat$mod_frac_case    <- dat$mod_counts_case    / dat$num_calls_case
  dat$mod_frac_control <- dat$mod_counts_control / dat$num_calls_control
  dat$meth_diff        <- dat$mod_frac_case - dat$mod_frac_control

  dat$p_val <- switch(calc_type,
    fast_fisher = .fast_fisher(
      q = dat$mod_counts_case,
      m = dat$mod_counts_case + dat$mod_counts_control,
      n = dat$c_counts_case   + dat$c_counts_control,
      k = dat$num_calls_case
    ),
    r_fisher = .r_fisher(
      a = dat$mod_counts_control,
      b = dat$mod_counts_case,
      c = dat$c_counts_control,
      d = dat$c_counts_case
    )
  )
  dat
}

.fast_fisher <- function(q, m, n, k) {
  # Calculate values once
  dhyper_val <- 0.5 * dhyper(q, m, n, k)

  pval_right <- phyper(q, m, n, k, lower.tail = FALSE) + dhyper_val
  pval_left  <- phyper(q - 1, m, n, k, lower.tail = TRUE) + dhyper_val

  # Return min tail * 2
  pmin(pval_right, pval_left) * 2
}

# old fast fisher
# .fast_fisher <- function(q, m, n, k)
# {
#   # derived from https://github.com/al2na/methylKit/issues/96
#
#   mat <- cbind(q, m, n, k)
#
#   apply(mat, 1,
#         \(qmnk)
#         {
#           dhyper_val <- 0.5 * dhyper(x = qmnk[1], m = qmnk[2],
#                                      n = qmnk[3], k = qmnk[4])
#
#           pval_right <- phyper(q = qmnk[1], m = qmnk[2],
#                                n = qmnk[3], k = qmnk[4],
#                                lower.tail = FALSE) + dhyper_val
#
#           pval_left  <- phyper(q = qmnk[1] - 1, m = qmnk[2],
#                                n = qmnk[3], k = qmnk[4],
#                                lower.tail = TRUE) + dhyper_val
#
#           return(ifelse(test = pval_right > pval_left,
#                         yes  = pval_left * 2,
#                         no   = pval_right * 2))
#         })
# }


.r_fisher <- function(a, b, c, d)
{
  mat <- cbind(a, b, c, d)

  apply(mat, 1,
        \(x)
        {
          fisher.test(matrix(x, 2))$p.val
        })
}


.calc_diff_logreg <- function(in_dat, group_vars)
{
  dat <- in_dat |>
    dplyr::select(dplyr::any_of(c(group_vars, "sample_name", "exp_group",
                                  "num_calls", "mod_counts"))) |>
    dplyr::collect()

  if (nrow(dat) == 0) return(data.frame())

  dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      num_samples_case    = sum(exp_group == "case"),
      num_samples_control = sum(exp_group == "control"),

      num_calls_case    = sum(num_calls[exp_group == "case"],    na.rm = TRUE),
      num_calls_control = sum(num_calls[exp_group == "control"], na.rm = TRUE),

      mod_counts_case    = sum(mod_counts[exp_group == "case"],   na.rm = TRUE),
      mod_counts_control = sum(mod_counts[exp_group == "control"], na.rm = TRUE),

      mod_frac_case = mean(mod_counts[exp_group == "case"] /
                             pmax(num_calls[exp_group == "case"], 1), na.rm = TRUE),
      mod_frac_control = mean(mod_counts[exp_group == "control"] /
                                pmax(num_calls[exp_group == "control"], 1), na.rm = TRUE),

      meth_diff = mean(mod_counts[exp_group == "case"] /
                         pmax(num_calls[exp_group == "case"], 1), na.rm = TRUE) -
                  mean(mod_counts[exp_group == "control"] /
                         pmax(num_calls[exp_group == "control"], 1), na.rm = TRUE),

      p_val = .logreg(mod_counts / pmax(num_calls, 1), num_calls, exp_group),

      .groups = "drop"
    )
}


# Weighted binomial logistic regression LRT.
# Fits intercept-only (null) vs intercept + group (alt), returns chi-sq p-value (1 df).
# mod_frac: per-sample modification fraction
# cov:      per-sample total read count (used as weights)
# exp_group: character vector, "case" or "control"
.logreg <- function(mod_frac, cov, exp_group)
{
  ok <- is.finite(mod_frac) & is.finite(cov) & cov > 0
  if (sum(ok) < 2 || length(unique(exp_group[ok])) < 2) return(NA_real_)

  mf  <- mod_frac[ok]
  w   <- cov[ok]
  grp <- as.integer(exp_group[ok] == "case")

  fit_null <- tryCatch(
    glm.fit(cbind(1),      mf, weights = w, family = binomial()),
    error = function(e) NULL
  )
  fit_alt <- tryCatch(
    glm.fit(cbind(1, grp), mf, weights = w, family = binomial()),
    error = function(e) NULL
  )

  if (is.null(fit_null) || is.null(fit_alt)) return(NA_real_)

  lrt_stat <- max(fit_null$deviance - fit_alt$deviance, 0)
  pchisq(lrt_stat, df = 1, lower.tail = FALSE)
}
