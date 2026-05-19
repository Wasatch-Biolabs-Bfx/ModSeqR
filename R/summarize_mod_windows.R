#' Summarize methylation in sliding windows
#'
#' Aggregates call-level rows from \code{input_table} (typically \code{"calls"})
#' into per-sample window summaries written to \code{output_table} (default \code{"windows"}).
#' Windows are created using a \strong{tiling + offsets} strategy so that every
#' \code{start} position is assigned to one or more windows depending on the
#' \code{step_size}. For each window the function computes:
#' total modification sites, total calls, per-class counts, and per-class fractions.
#'
#' @section How modification codes work:
#' Pass \code{mod_code} as single codes (e.g. \code{"m"}, \code{"h"}, \code{"a"})
#' or combinations with \code{"+"} (e.g. \code{"m + h"}). Labels are created by
#' removing spaces and \code{"+"} (e.g. \code{"m + h"} → \code{"mh"}). For each
#' label the table includes \code{<label>_counts} and \code{<label>_frac}. The
#' unmodified class is defined by \code{unmod_code} (default \code{"-"}), named
#' using \code{unmod_label} (default \code{"c"} → \code{c_counts}, \code{c_frac}).
#'
#' @param mod_db Path to a \code{.mod.db} DuckDB file or a \code{"mod_db"} object.
#'   A connection is opened via internal helpers and closed/cleaned on return.
#' @param input_table Source table containing call-level records (default \code{"calls"}).
#'   Must contain at least: \code{sample_name}, \code{chrom}, \code{start}, \code{call_code}.
#' @param output_table Destination window table name (default \code{"windows"}).
#' @param window_size Integer window width in bases (default \code{1000}).
#' @param step_size Step, in bases, used to create staggered window offsets
#'   (default \code{10}). Offsets are \code{seq(1, window_size - 1, by = step_size)}.
#' @param chrs Optional character vector of chromosome filters. If \code{NULL} (default),
#'   all chromosomes present in the data are used.
#' @param samples Optional character vector of \code{sample_name}s to include. If \code{NULL},
#'   all samples present in \code{input_table} are processed.
#' @param mod_code Character vector of modification specs to count (single codes or
#'   \code{"code1 + code2"} combinations). Default \code{c("m","h","m + h")}.
#' @param unmod_code Call code representing unmodified (default \code{"-"}).
#' @param unmod_label Label used to name unmodified columns (default \code{"c"}).
#' @param min_num_calls Minimum total calls required for a window to be written
#'   (default \code{1}). Windows below this threshold are skipped.
#' @param batch_size Ignored. Kept for backward compatibility. The function now
#'   processes one chromosome at a time and issues a \code{CHECKPOINT} after each,
#'   so manual batching is unnecessary.
#' @param temp_dir Directory for DuckDB temporary files (default \code{tempdir()}).
#' @param threads Integer DuckDB thread count. If \code{NULL}, an internal heuristic
#'   (typically all-but-one core) is used.
#' @param memory_limit DuckDB memory limit string (e.g. \code{"16384MB"}).
#'   If \code{NULL}, an internal heuristic (~80\% of RAM) is used.
#' @param overwrite If \code{TRUE} and \code{output_table} exists, it is dropped before writing.
#'
#' @details
#' The function processes one chromosome at a time. For each chromosome it:
#' \enumerate{
#'   \item Aggregates per-position call counts for all samples into a persistent
#'         staging table (not a temporary table, so DuckDB can page it to disk).
#'   \item Handles all window offsets in a single SQL pass using a
#'         \code{CROSS JOIN} with an inline offset list (\code{unnest}), avoiding
#'         \eqn{N_{\text{offsets}}} redundant scans of the staging data.
#'   \item Inserts the window summaries into \code{output_table} and issues a
#'         \code{CHECKPOINT} to flush completed work to disk before moving on.
#' }
#' Window assignment uses: \deqn{win\_start = start - ((start - offset) \bmod window\_size).}
#' Resource pragmas (\code{temp_directory}, \code{threads}, \code{memory_limit}) are set
#' via internal heuristics unless overridden.
#'
#' @return (Invisibly) a \code{"mod_db"} object pointing to the same DB file with
#'   \code{current_table} set to \code{output_table}. The created table has columns:
#'   \itemize{
#'     \item \code{sample_name}, \code{chrom}, \code{start}, \code{end},
#'           \code{num_sites}, \code{num_calls},
#'     \item for each label in \code{c(unmod_label, parsed(mod_code))}:
#'           \code{<label>_counts}, \code{<label>_frac}.
#'   }
#'   \code{last_result} is set to a tibble with columns \code{sample_name} and \code{n}
#'   (row count per sample in the output table).
#'
#' @examples
#' \dontrun{
#' # Default m/h windows (1kb windows, 10bp staggered offsets)
#' summarize_mod_windows("my_db.mod.db")
#'
#' # Custom mod codes with a novel 'a' code and stricter filtering
#' summarize_mod_windows(
#'   mod_db        = "my_db.mod.db",
#'   mod_code      = c("a", "m + h"),
#'   min_num_calls = 25,
#'   window_size   = 2000,
#'   step_size     = 20
#' )
#'
#' # Limit to selected samples and chromosomes; recreate table if present
#' summarize_mod_windows(
#'   mod_db       = "my_db.mod.db",
#'   samples      = c("Astrocytes","Blood_Plasma"),
#'   chrs         = c("chr1","chrX"),
#'   overwrite    = TRUE
#' )
#' }
#'
#' @seealso
#' \code{\link{make_mod_db}},
#' \code{\link{summarize_mod_positions}},
#' \code{\link{summarize_mod_regions}},
#' \code{\link{calc_mod_diff}}
#'
#' @importFrom DBI dbExecute dbExistsTable dbGetQuery dbQuoteIdentifier dbRemoveTable
#' @importFrom glue glue
#' @export

summarize_mod_windows <- function(mod_db,
                                  input_table  = "calls",
                                  output_table = "windows",
                                  window_size = 1000,
                                  step_size = 10,
                                  chrs = NULL,
                                  samples = NULL,             # NULL = all samples
                                  mod_code    = c("m", "h", "m + h"),
                                  unmod_code  = "-",
                                  unmod_label = "c",
                                  min_num_calls = 1,
                                  batch_size = NULL,
                                  temp_dir = tempdir(),
                                  threads = NULL,             # default: all-but-one
                                  memory_limit = NULL,        # default: ~80% RAM
                                  overwrite = TRUE)
{
  start_time <- Sys.time()
  mod_db <- ModSeqR:::.modhelper_connectDB(mod_db)
  
  # Resource caps
  caps <- .auto_duckdb_resource_caps(0.80)
  thr  <- if (is.null(threads)) caps$threads else threads
  mem  <- if (is.null(memory_limit)) caps$memory_limit else memory_limit
  
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  DBI::dbExecute(mod_db$con, sprintf("PRAGMA temp_directory='%s';", temp_dir))
  DBI::dbExecute(mod_db$con, sprintf("PRAGMA memory_limit='%s';", mem))
  DBI::dbExecute(mod_db$con, sprintf("PRAGMA threads=%d;", thr))
  
  in_id  <- as.character(DBI::dbQuoteIdentifier(mod_db$con, input_table))
  out_id <- as.character(DBI::dbQuoteIdentifier(mod_db$con, output_table))

  if (DBI::dbExistsTable(mod_db$con, output_table) && overwrite)
    DBI::dbRemoveTable(mod_db$con, output_table)

  # Build sample filter clause for SQL pushdown (avoids R-side sample loop)
  samp_clause <- if (!is.null(samples)) {
    vals <- paste(sprintf("'%s'", gsub("'", "''", samples)), collapse = ", ")
    sprintf("AND sample_name IN (%s)", vals)
  } else ""

  specs   <- .parse_mod_specs(mod_code)
  cntsql  <- .build_pos_count_sql(unmod_code, unmod_label, specs)
  countsS <- cntsql$select_counts_pos
  labels  <- cntsql$labels_all

  cat("Summarizing Windows...\n")

  # Pre-create output schema with dynamic columns
  count_nulls <- paste(sprintf("CAST(NULL AS BIGINT) AS %s_counts", labels), collapse = ",\n      ")
  frac_nulls  <- paste(sprintf("CAST(NULL AS DOUBLE) AS %s_frac", labels),  collapse = ",\n      ")
  schema_sql <- glue::glue("
    CREATE TABLE IF NOT EXISTS {out_id} AS
    SELECT
      CAST(NULL AS VARCHAR) AS sample_name,
      CAST(NULL AS VARCHAR) AS chrom,
      CAST(NULL AS BIGINT)  AS start,
      CAST(NULL AS BIGINT)  AS \"end\",
      CAST(NULL AS BIGINT)  AS num_sites,
      CAST(NULL AS BIGINT)  AS num_calls,
      {count_nulls},
      {frac_nulls}
    WHERE 1=0;
  ")
  DBI::dbExecute(mod_db$con, schema_sql)

  # Per-label SQL fragments for window aggregation
  p_counts   <- paste(sprintf("p.%s_counts", labels), collapse = ", ")
  sum_counts <- paste(sprintf("SUM(%s_counts) AS %s_counts", labels, labels),
                      collapse = ",\n        ")
  frac_exprs <- paste(sprintf(
    "CASE WHEN SUM(num_calls) = 0 THEN NULL\n             ELSE SUM(%s_counts) * 1.0 / SUM(num_calls) END AS %s_frac",
    labels, labels), collapse = ",\n        ")

  offsets     <- seq(1L, window_size - 1L, by = step_size)
  offset_list <- paste(offsets, collapse = ", ")

  # Determine chromosome list (respects chrs filter and sample filter)
  if (is.null(chrs)) {
    chrs <- DBI::dbGetQuery(
      mod_db$con,
      sprintf("SELECT DISTINCT chrom FROM %s WHERE start > 0 %s ORDER BY chrom",
              in_id, samp_clause)
    )[, 1]
  }

  message("Processing ", length(chrs), " chromosomes (",
          length(offsets), " offsets in single pass)...")

  for (ci in seq_along(chrs)) {
    chr     <- chrs[ci]
    chr_esc <- gsub("'", "''", chr)

    # Aggregate per-position counts for all (filtered) samples on this chromosome.
    # Persistent table (not TEMP) lets DuckDB page it to disk under memory pressure.
    DBI::dbExecute(mod_db$con, "DROP TABLE IF EXISTS _staging_chr_pos")
    DBI::dbExecute(mod_db$con, sprintf(
      "CREATE TABLE _staging_chr_pos AS
       SELECT sample_name, start,
         COUNT(*) AS num_calls,
         %s
       FROM %s
       WHERE chrom = '%s' AND start > 0 %s
       GROUP BY sample_name, start",
      countsS, in_id, chr_esc, samp_clause
    ))

    # All offsets handled in one INSERT via CROSS JOIN + unnest,
    # avoiding N redundant scans of the staging table.
    DBI::dbExecute(mod_db$con, sprintf(
      "INSERT INTO %s
       WITH offsets(win_offset) AS (
         SELECT unnest([%s])
       ),
       window_map AS (
         SELECT p.sample_name,
           (p.start - ((p.start - o.win_offset) %% %d)) AS win_start,
           p.num_calls, %s
         FROM _staging_chr_pos p CROSS JOIN offsets o
       )
       SELECT sample_name, '%s' AS chrom,
         win_start                    AS start,
         win_start + %d - 1           AS \"end\",
         COUNT(*)                     AS num_sites,
         SUM(num_calls)               AS num_calls,
         %s,
         %s
       FROM window_map
       GROUP BY sample_name, win_start
       HAVING SUM(num_calls) >= %d",
      out_id, offset_list, window_size, p_counts,
      chr_esc, window_size, sum_counts, frac_exprs, min_num_calls
    ))

    DBI::dbExecute(mod_db$con, "DROP TABLE IF EXISTS _staging_chr_pos")
    DBI::dbExecute(mod_db$con, "CHECKPOINT")
    message("  [", ci, "/", length(chrs), "] ", chr, " done")
  }

  end_time <- Sys.time()
  message("Windows table created as ", output_table,
          " (", round(as.numeric(end_time - start_time, "mins"), 2), " min).")
  
  mod_db$last_result <- dplyr::tbl(mod_db$con, output_table) |>
    dplyr::count(sample_name) |>
    dplyr::collect()
  mod_db$current_table <- output_table
  mod_db <- ModSeqR:::.modhelper_cleanup(mod_db)
  invisible(mod_db)
}