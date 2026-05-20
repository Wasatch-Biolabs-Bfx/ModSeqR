#' Summarize methylation by annotated regions
#'
#' Aggregates call-level rows from \code{input_table} (typically \code{"calls"})
#' into per-sample region summaries written to \code{output_table} (default
#' \code{"regions"}). Regions are provided via a BED/TSV/CSV file with columns
#' \code{chrom}, \code{start}, \code{end}, and optional \code{region_name}.
#' For each region the function computes:
#' number of unique positions where modification could occur (rows), total calls, per-class counts, and per-class
#' fractions.
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
#'   A connection is opened via internal helpers and cleaned on return.
#' @param input_table Source table containing call-level records (default \code{"calls"}).
#'   Must contain at least: \code{sample_name}, \code{chrom}, \code{start}, \code{end}, \code{call_code}.
#' @param output_table Destination table name (default \code{"regions"}).
#' @param region_file BED/TSV/CSV path with columns \code{chrom}, \code{start}, \code{end}
#'   and optional \code{region_name}. If missing, \code{region_name} is synthesized as
#'   \code{"chrom_start_end"}.
#' @param join Join type between positions and regions: one of \code{"inner"}, \code{"left"},
#'   or \code{"right"} (default \code{"inner"}).
#' @param chrs Optional character vector of chromosome filters. If \code{NULL} (default),
#'   all chromosomes present in the data are used.
#' @param samples Optional character vector of \code{sample_name}s to include. If \code{NULL},
#'   all samples present in \code{input_table} are processed.
#' @param mod_code Character vector of modification specs to count (single codes or
#'   \code{"code1 + code2"} combinations). Default \code{c("m","h","m + h")}.
#' @param unmod_code Call code representing unmodified (default \code{"-"}).
#' @param unmod_label Label used to name unmodified columns (default \code{"c"}).
#' @param min_num_calls Minimum total calls required at the \emph{region} level to be written
#'   (default \code{1}). Regions below this threshold are skipped.
#' @param batch_size Ignored. Kept for backward compatibility. The function now
#'   processes one chromosome at a time (positions and annotation), issuing a
#'   \code{CHECKPOINT} after each sample, so manual batching is unnecessary.
#' @param temp_dir Directory for DuckDB temporary files (default \code{tempdir()}).
#' @param threads Integer DuckDB thread count. If \code{NULL}, an internal heuristic
#'   (typically all-but-one core) is used.
#' @param memory_limit DuckDB memory limit string (e.g. \code{"16384MB"}).
#'   If \code{NULL}, an internal heuristic (~80\% of RAM) is used.
#' @param overwrite If \code{TRUE} and \code{output_table} exists, it is dropped before writing.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Reads \code{region_file} (CSV/TSV/BED) and normalizes columns. If \code{region_name}
#'         is absent, it is synthesized. Basic chromosome-prefix harmonization is performed when
#'         DB positions and annotation disagree on presence of a \code{"chr"} prefix.
#'   \item Configures DuckDB pragmas (\code{temp_directory}, \code{threads}, \code{memory_limit}).
#'   \item For each sample, iterates chromosome by chromosome: aggregates positions into a
#'         persistent staging table (not a temporary table, so DuckDB can page it to disk),
#'         then immediately joins to the annotation for that chromosome and inserts results
#'         into \code{output_table}. A \code{CHECKPOINT} is issued after each sample.
#' }
#'
#' @return (Invisibly) a \code{"mod_db"} object pointing to the same DB file with
#'   \code{current_table} set to \code{output_table}. The created table has columns:
#'   \itemize{
#'     \item \code{sample_name}, \code{region_name}, \code{chrom}, \code{start}, \code{end},
#'           \code{num_sites}, \code{num_calls},
#'     \item for each label in \code{c(unmod_label, parsed(mod_code))}:
#'           \code{<label>_counts}, \code{<label>_frac}.
#'   }
#'   \code{last_result} is set to a tibble with columns \code{sample_name} and \code{n}
#'   (row count per sample in the output table).
#'
#' @examples
#' \dontrun{
#' # Default m/h summary by regions
#' summarize_mod_regions(
#'   mod_db      = "my_db.mod.db",
#'   region_file = "islands_hg38.bed"
#' )
#'
#' # Novel 'a' code and m+h combination, left join to keep empty regions
#' summarize_mod_regions(
#'   mod_db      = "my_db.mod.db",
#'   region_file = "islands_hg38.csv",
#'   mod_code    = c("a","m + h"),
#'   join        = "left",
#'   min_num_calls = 10
#' )
#' }
#'
#' @seealso
#' \code{\link{make_mod_db}},
#' \code{\link{summarize_mod_positions}},
#' \code{\link{summarize_mod_windows}},
#' \code{\link{calc_mod_diff}}
#'
#' @importFrom DBI dbExecute dbExistsTable dbGetQuery dbWriteTable dbQuoteIdentifier dbRemoveTable
#' @importFrom glue glue
#' @importFrom readr read_csv read_tsv
#' @importFrom tools file_ext
#' @export

summarize_mod_regions <- function(mod_db,
                                  input_table  = "calls",
                                  output_table = "regions",
                                  region_file,
                                  join = c("inner","left","right"),
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
  join <- match.arg(join)
  
  # ---- Read annotation (CSV/TSV/BED) ------------------------------------------
  ext <- tools::file_ext(region_file)
  if (tolower(ext) == "csv") {
    annotation <- readr::read_csv(
      region_file,
      col_names = c("chrom","start","end","region_name"),
      show_col_types = FALSE
    )
  } else if (tolower(ext) %in% c("tsv","bed")) {
    annotation <- readr::read_tsv(
      region_file,
      col_names = c("chrom","start","end","region_name"),
      show_col_types = FALSE
    )
  } else {
    stop("Invalid file type: ", ext, ". Only CSV, TSV, or BED are supported.")
  }
  
  # If file has a header row that duplicates colnames, drop it
  if (nrow(annotation) > 0 && tolower(annotation$chrom[1]) %in% c("chr","chrom")) {
    annotation <- annotation[-1, , drop = FALSE]
  }
  
  # Synthesize region_name if missing
  if (!"region_name" %in% names(annotation) || all(is.na(annotation$region_name))) {
    annotation$region_name <- paste(annotation$chrom, annotation$start, annotation$end, sep = "_")
  }
  
  # ---- Open DB and set resource caps ------------------------------------------
  mod_db <- ModSeqR:::.modhelper_connectDB(mod_db)
  
  caps <- .auto_duckdb_resource_caps(0.80)
  thr  <- if (is.null(threads)) caps$threads else threads
  mem  <- if (is.null(memory_limit)) caps$memory_limit else memory_limit
  
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  DBI::dbExecute(.get_con(mod_db), sprintf("PRAGMA temp_directory='%s';", temp_dir))
  DBI::dbExecute(.get_con(mod_db), sprintf("PRAGMA memory_limit='%s';", mem))
  DBI::dbExecute(.get_con(mod_db), sprintf("PRAGMA threads=%d;", thr))
  
  in_id  <- as.character(DBI::dbQuoteIdentifier(.get_con(mod_db), input_table))
  out_id <- as.character(DBI::dbQuoteIdentifier(.get_con(mod_db), output_table))
  
  if (DBI::dbExistsTable(.get_con(mod_db), output_table) && overwrite)
    DBI::dbRemoveTable(.get_con(mod_db), output_table)
  
  # ---- Determine sample list ---------------------------------------------------
  samp_query <- sprintf("SELECT DISTINCT sample_name FROM %s WHERE sample_name IS NOT NULL", in_id)
  all_samps <- DBI::dbGetQuery(.get_con(mod_db), samp_query)[,1]
  if (!is.null(samples)) {
    all_samps <- intersect(all_samps, samples)
  }
  if (length(all_samps) == 0) stop("No samples found.")
  
  # ---- Build dynamic count SQL like positions/windows --------------------------
  specs   <- .parse_mod_specs(mod_code)
  cntsql  <- .build_pos_count_sql(unmod_code, unmod_label, specs)
  countsS <- cntsql$select_counts_pos
  labels  <- cntsql$labels_all
  
  cat("Summarizing Regions...\n")
  
  # ---- Prepare output schema with dynamic columns ------------------------------
  count_nulls <- paste(sprintf("CAST(NULL AS BIGINT) AS %s_counts", labels), collapse = ",\n      ")
  frac_nulls  <- paste(sprintf("CAST(NULL AS DOUBLE) AS %s_frac",  labels), collapse = ",\n      ")
  schema_sql <- glue::glue("
    CREATE TABLE IF NOT EXISTS {out_id} AS
    SELECT
      CAST(NULL AS VARCHAR) AS sample_name,
      CAST(NULL AS VARCHAR) AS region_name,
      CAST(NULL AS VARCHAR) AS chrom,
      CAST(NULL AS BIGINT)  AS start,
      CAST(NULL AS BIGINT)  AS \"end\",
      CAST(NULL AS BIGINT)  AS num_sites,
      CAST(NULL AS BIGINT)  AS num_calls,
      {count_nulls},
      {frac_nulls}
    WHERE 1=0;
  ")
  DBI::dbExecute(.get_con(mod_db), schema_sql)
  
  # ---- Upload annotation to temp table -----------------------------------------
  DBI::dbExecute(.get_con(mod_db), "DROP TABLE IF EXISTS temp_annotation;")
  DBI::dbWriteTable(.get_con(mod_db), "temp_annotation", annotation, temporary = TRUE)
  
  # If no chromosomes specified, use all unique chroms in the data
  if (is.null(chrs)) {
    chrs <- DBI::dbGetQuery(
      .get_con(mod_db),
      sprintf("SELECT DISTINCT chrom FROM %s ORDER BY chrom", in_id)
    )[, 1]
  }
  
  # ---- Chromosome filter clause ------------------------------------------------
  chr_clause <- .chrom_filter_sql(chrs)
  
  # ---- Harmonize 'chr' prefix if needed (simple heuristic) ---------------------
  # Check whether positions have 'chr' prefix
  has_chr_positions <- DBI::dbGetQuery(.get_con(mod_db), sprintf("
    SELECT CASE WHEN EXISTS (
      SELECT 1 FROM %s WHERE chrom LIKE 'chr%%' LIMIT 1
    ) THEN 1 ELSE 0 END AS v
  ", in_id))$v[1] == 1
  
  has_chr_annot <- any(grepl("^chr", annotation$chrom))
  
  if (has_chr_positions && !has_chr_annot) {
    # add 'chr' to annotation
    DBI::dbExecute(.get_con(mod_db), "UPDATE temp_annotation SET chrom = 'chr' || CAST(chrom AS VARCHAR);")
  } else if (!has_chr_positions && has_chr_annot) {
    # strip 'chr' from annotation
    DBI::dbExecute(.get_con(mod_db), "UPDATE temp_annotation SET chrom = REGEXP_REPLACE(chrom, '^chr', '');")
  }

  DBI::dbExecute(.get_con(mod_db), "DROP TABLE IF EXISTS temp_annotation_batched;")
  # Validate annotation was loaded
  total_regions <- DBI::dbGetQuery(.get_con(mod_db),
                                   "SELECT COUNT(*) AS n FROM temp_annotation")$n[1]
  if (!is.finite(total_regions) || total_regions == 0) {
    stop("No regions found in region_file after parsing.")
  }

  # Build dynamic SUMs and fractions at region level (computed once, reused per chr)
  sum_counts <- paste(sprintf("COALESCE(SUM(p.%s_counts), 0) AS %s_counts", labels, labels),
                      collapse = ",\n          ")
  frac_cols  <- paste(sprintf(
    "CASE WHEN COALESCE(SUM(p.num_calls),0) = 0 THEN NULL ELSE COALESCE(SUM(p.%s_counts),0) * 1.0 / COALESCE(SUM(p.num_calls),0) END AS %s_frac",
    labels, labels), collapse = ",\n          ")
  join_kw <- switch(join, inner = "JOIN", left = "LEFT JOIN", right = "RIGHT JOIN")

  # ---- Process per sample, one chromosome at a time ----------------------------
  # Persistent staging table (not TEMP) lets DuckDB page positions to disk.
  # Annotation is filtered per-chromosome in SQL, avoiding a batching loop.
  # CHECKPOINT after each sample releases memory before moving on.
  for (samp in all_samps) {
    samp_esc <- gsub("'", "''", samp)

    chrom_q <- glue::glue("\
      SELECT DISTINCT chrom
      FROM {in_id}
      WHERE sample_name = '{samp_esc}' AND start > 0{chr_clause}
      ORDER BY chrom
    ")
    sample_chroms <- DBI::dbGetQuery(.get_con(mod_db), chrom_q)[, 1]
    if (length(sample_chroms) == 0) next

    for (chr in sample_chroms) {
      chr_esc <- gsub("'", "''", chr)

      # Aggregate per-position counts into a persistent staging table
      DBI::dbExecute(.get_con(mod_db), "DROP TABLE IF EXISTS _staging_pos")
      DBI::dbExecute(.get_con(mod_db), sprintf(
        "CREATE TABLE _staging_pos AS
         SELECT sample_name, chrom, start, \"end\",
           COUNT(*) AS num_calls,
           %s
         FROM %s
         WHERE sample_name = '%s' AND chrom = '%s' AND start > 0
         GROUP BY sample_name, chrom, start, \"end\"",
        countsS, in_id, samp_esc, chr_esc
      ))

      # Join positions to annotation for this chromosome and insert into output
      DBI::dbExecute(.get_con(mod_db), sprintf(
        "INSERT INTO %s
         SELECT
           p.sample_name,
           a.region_name,
           a.chrom,
           a.start,
           a.\"end\",
           COUNT(*)                       AS num_sites,
           COALESCE(SUM(p.num_calls), 0)  AS num_calls,
           %s,
           %s
         FROM _staging_pos p
         %s temp_annotation a
           ON p.chrom = a.chrom
          AND CAST(p.start AS DOUBLE) BETWEEN CAST(a.start AS DOUBLE) AND CAST(a.\"end\" AS DOUBLE)
         WHERE a.chrom = '%s'
         GROUP BY p.sample_name, a.region_name, a.chrom, a.start, a.\"end\"
         HAVING COALESCE(SUM(p.num_calls), 0) >= %d",
        out_id, sum_counts, frac_cols, join_kw, chr_esc, min_num_calls
      ))

      DBI::dbExecute(.get_con(mod_db), "DROP TABLE IF EXISTS _staging_pos")
    }

    DBI::dbExecute(.get_con(mod_db), "CHECKPOINT")
    message("  Sample '", samp, "' done")
  }
  
  # ---- Finish ------------------------------------------------------------------
  message("Regions table created as ", output_table,
          " (", round(as.numeric(Sys.time() - start_time, "secs"), 1), "s).")
  
  mod_db$last_result <- dplyr::tbl(.get_con(mod_db), output_table) |>
    dplyr::count(sample_name) |>
    dplyr::collect()
  mod_db$current_table <- output_table
  mod_db <- ModSeqR:::.modhelper_cleanup(mod_db)
  invisible(mod_db)
}
