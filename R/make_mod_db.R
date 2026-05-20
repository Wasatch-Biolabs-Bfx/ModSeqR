#' Create a Modifications DuckDB from Parquet CH3 files (with optional sample naming)
#'
#' Build a DuckDB database containing filtered modification call data from one or
#' more \code{.ch3} parquet files. Inputs may be individual files, directories
#' (expanded to all \code{*.ch3} files), or a **named character vector** where
#' names are used as \code{sample_name}s in the output.
#'
#' The function opens a private DuckDB connection for the build, writes the
#' \code{calls} table, then closes that connection and returns a
#' \emph{disconnected} \code{mod_db} object. The connection is opened lazily
#' the first time a pipeline function is called, or explicitly via
#' \code{\link{connect_mod_db}()}.
#'
#' @section Input forms:
#' \itemize{
#'   \item \strong{Files}: \code{c("a.ch3", "b.ch3")}
#'   \item \strong{Directories}: \code{c("dir_of_ch3s/")} (expands to all \code{*.ch3})
#'   \item \strong{Named files/dirs}: \code{c(SampleA = "a.ch3", SampleB = "dir/")}
#'         — names become \code{sample_name}. If a name is not provided, the
#'         filename stem (without \code{.ch3}) is used.
#' }
#'
#' @param ch3_files Character vector of CH3 parquet file paths and/or directories.
#'   May be a \emph{named} vector to assign \code{sample_name}s explicitly; any
#'   entry of the form \code{NAME=PATH} is also accepted. Directories are scanned
#'   (non-recursively) for \code{*.ch3} files. Must not be empty.
#' @param db_name Path (without or with \code{.mod.db} extension) for the DuckDB
#'   database to be created; \code{.mod.db} is appended if missing.
#' @param chrom Optional chromosome filter. Either a single string (e.g.,
#'   \code{"chr1"}) or a character vector (e.g., \code{c("chr1","chr2","chrX")}).
#'   If \code{NULL}, all chromosomes are included.
#' @param min_read_length Minimum read length to keep (default \code{50}).
#' @param min_call_prob Minimum call probability to keep (default \code{0.9}).
#' @param min_base_qual Minimum base quality to keep (default \code{10}).
#' @param flag Optional numeric flag value to require; if \code{NULL}, no flag filter.
#' @param threads Integer DuckDB thread count. If \code{NULL}, an internal heuristic
#'   (all-but-one core) is used.
#' @param memory_limit DuckDB memory limit string (e.g. \code{"16384MB"}).
#'   If \code{NULL}, an internal heuristic (~75\% of detected RAM) is used.
#' @param temp_dir Directory DuckDB uses for spill-to-disk scratch files.
#'   If \code{NULL}, a subdirectory of \code{tempdir()} is used.
#' @param batch_size Optional integer number of \code{.ch3} files to process per batch.
#'   If \code{NULL}, all files are processed in one batch.
#'
#' @details
#' \strong{What it does}
#' \itemize{
#'   \item Expands \code{ch3_files} (handling directories and named entries) into a mapping
#'         of source files and optional \code{sample_name}s.
#'   \item Configures DuckDB resource limits (memory, threads, temp directory) at
#'         connection time so spill-to-disk is active from the first query.
#'   \item Drops any existing tables in the target DB.
#'   \item Reads all input \code{.ch3} parquet files in a single pass and creates a
#'         table \code{calls} with columns:
#'         \code{sample_name}, \code{chrom}, \code{start}, \code{end}, \code{read_position},
#'         \code{call_code}, \code{read_length}, \code{call_prob}, \code{base_qual}, \code{flag}.
#'         When names are not given for inputs, \code{sample_name} defaults to the file stem.
#'         File reading can be split into batches using \code{batch_size}.
#'   \item Applies pushdown filters based on \code{chrom}, \code{min_read_length},
#'         \code{min_call_prob}, \code{min_base_qual}, and \code{flag}.
#' }
#'
#' \strong{Side effects and performance}
#' \itemize{
#'   \item Creates (or overwrites) a DuckDB file at \code{db_name}.
#'   \item Uses a temp directory for DuckDB spills (controlled by \code{temp_dir}).
#'   \item A temporary in-memory table \code{file_map} may be created for input mapping.
#' }
#'
#' @return (Invisibly) a disconnected \code{mod_db} object with five slots:
#'   \code{db_file}, \code{current_table} (\code{"calls"}), \code{last_result}
#'   (tibble of row counts per sample), \code{config}, and \code{.conn_env}
#'   (connection environment, initially closed). Pass the result to any pipeline
#'   function — the connection opens automatically on first use — or call
#'   \code{\link{connect_mod_db}()} to open it explicitly.
#'
#' @examples
#' \dontrun{
#' # 1) Directory of CH3 files (non-recursive scan for *.ch3)
#' mod_db <- make_mod_db(ch3_files = "path/to/ch3_dir",
#'                        db_name   = "my_db")
#'
#' # 2) Explicit files (auto-sample names from stems)
#' mod_db <- make_mod_db(ch3_files = c("A.ch3", "B.ch3"),
#'                        db_name   = "two_samples.mod.db",
#'                        min_read_length = 100, min_base_qual = 10)
#'
#' # 3) Named inputs (sample_name set from names)
#' mod_db <- make_mod_db(
#'   ch3_files = c(
#'     Sample1 = "../CH3/Sample1.ch3",
#'     Sample2 = "../CH3/Sample2.ch3"
#'   ),
#'   db_name = "My_DB",
#'   min_base_qual = 10,
#'   min_read_length = 100
#' )
#'
#' # 4) Filter to specific chromosomes, explicit resource caps
#' mod_db <- make_mod_db(
#'   ch3_files    = c(S1 = "A.ch3", S2 = "B.ch3"),
#'   db_name      = "chr1_chrX_only",
#'   chrom        = c("chr1","chrX"),
#'   memory_limit = "32GB",
#'   threads      = 8
#' )
#'
#' # 5) Connect to the same database later in a new session
#' mod_db <- connect_mod_db("My_DB.mod.db")
#' }
#'
#' @seealso
#' \code{\link{connect_mod_db}}, \code{\link{summarize_mod_positions}},
#' \code{\link{summarize_mod_regions}}, \code{\link{summarize_mod_windows}},
#' \code{\link{get_mod_dbinfo}}, \code{\link{get_mod_tableinfo}},
#' \code{\link{calc_mod_diff}}
#'
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbListTables dbWriteTable
#'   dbQuoteIdentifier dbGetQuery
#' @importFrom duckdb duckdb
#' @importFrom glue glue
#' @importFrom parallel detectCores
#'
#' @export


make_mod_db <- function(ch3_files,
                        db_name,
                        chrom = NULL,
                        min_read_length = 50,
                        min_call_prob  = 0.9,
                        min_base_qual  = 10,
                        flag = NULL,
                        threads = NULL,
                        memory_limit = NULL,
                        temp_dir = NULL,
                        batch_size = NULL)
{
  start_time <- Sys.time()
  if (length(ch3_files) == 0) stop("No files provided.")

  # --- Accept named vectors: convert names to "name=path" entries -----------------
  if (!is.null(names(ch3_files))) {
    nm <- names(ch3_files)
    named_idx <- nzchar(nm)
    ch3_files[named_idx] <- paste0(nm[named_idx], "=", ch3_files[named_idx])
    names(ch3_files) <- NULL
  }

  # --- resolve entries: expand dirs and parse `name=path` -------------------------
  parse_entry <- function(entry) {
    if (grepl("=", entry, fixed = TRUE)) {
      parts <- strsplit(entry, "=", fixed = TRUE)[[1]]
      nm <- trimws(parts[1])
      p  <- trimws(paste(parts[-1], collapse = "="))
    } else {
      nm <- NA_character_
      p  <- entry
    }
    list(name = nm, path = p)
  }
  entries <- lapply(ch3_files, parse_entry)

  expand_paths <- function(nm, p) {
    if (!file.exists(p)) stop("Path does not exist: ", p)
    if (dir.exists(p)) {
      fs <- list.files(p, pattern = "\\.ch3$", full.names = TRUE, recursive = FALSE)
      if (length(fs) == 0) stop("No .ch3 files in directory: ", p)
      data.frame(sample_name = if (!is.na(nm)) nm else NA_character_,
                 file = normalizePath(fs, winslash = "/"),
                 stringsAsFactors = FALSE)
    } else {
      data.frame(sample_name = nm,
                 file = normalizePath(p, winslash = "/"),
                 stringsAsFactors = FALSE)
    }
  }

  df_files <- do.call(
    rbind,
    Map(expand_paths, lapply(entries, `[[`, "name"), lapply(entries, `[[`, "path"))
  )
  if (nrow(df_files) == 0) stop("No .ch3 files found.")

  # --- derive robust fallback names in R (not SQL) ---------------------------------
  derive_sample_from_path <- function(p) {
    b    <- basename(p)
    stem <- sub("(?i)\\.ch3$", "", b, perl = TRUE)
    stem <- sub("-[0-9]+$", "", stem, perl = TRUE)
    stem
  }

  need_name <- is.na(df_files$sample_name) | !nzchar(df_files$sample_name)
  df_files$sample_name[need_name] <- vapply(
    df_files$file[need_name], derive_sample_from_path, character(1)
  )
  df_files$base <- basename(df_files$file)

  # --- DB setup -------------------------------------------------------------------
  if (!grepl("\\.mod\\.db$", db_name)) db_name <- paste0(db_name, ".mod.db")
  cat("Building Database...\n")

  file_batch_size <- if (is.null(batch_size)) nrow(df_files) else as.integer(batch_size[1])
  if (!is.finite(file_batch_size) || file_batch_size < 1)
    stop("batch_size must be an integer >= 1 when provided.")

  # Open a private build connection with resource limits configured at engine level.
  cfg <- .resolve_duckdb_config(list(memory_limit = memory_limit,
                                     temp_dir     = temp_dir,
                                     threads      = threads))
  con <- DBI::dbConnect(duckdb::duckdb(db_name, config = cfg), read_only = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Drop existing tables
  for (tbl in DBI::dbListTables(con)) {
    DBI::dbExecute(con, paste0("DROP TABLE ", DBI::dbQuoteIdentifier(con, tbl)))
  }

  # --- small mapping table (path -> optional sample_name) -------------------------
  DBI::dbWriteTable(con, "file_map", df_files, temporary = TRUE, overwrite = TRUE)

  # --- filters & columns (pushdown) -----------------------------------------------
  esc <- function(x) gsub("'", "''", x)
  filters <- c()
  if (!is.null(chrom)) {
    if (length(chrom) == 1) {
      filters <- c(filters, paste0("chrom = '", esc(chrom), "'"))
    } else {
      filters <- c(filters, paste0("chrom IN (", paste(sprintf("'%s'", esc(chrom)), collapse = ", "), ")"))
    }
  }
  filters <- c(filters, paste0("read_length >= ", as.numeric(min_read_length)))
  filters <- c(filters, paste0("call_prob  >= ", as.numeric(min_call_prob)))
  filters <- c(filters, paste0("base_qual  >= ", as.numeric(min_base_qual)))
  if (!is.null(flag)) filters <- c(filters, paste0("flag = ", as.numeric(flag)))
  where_clause <- if (length(filters)) paste("WHERE", paste(filters, collapse = " AND ")) else ""

  file_list_sql <- paste0("['", paste(esc(df_files$file), collapse = "','"), "']")

  # --- detect schema compatibility across files ------------------------------------
  schema_all <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM read_parquet(%s, filename = TRUE, union_by_name = TRUE) LIMIT 0",
      file_list_sql
    )
  )

  union_has_read_position <- "read_position" %in% names(schema_all)
  all_have_read_position  <- TRUE
  if (union_has_read_position) {
    all_have_read_position <- all(vapply(df_files$file, function(fp) {
      one_sql <- sprintf(
        "SELECT * FROM read_parquet(['%s']) LIMIT 0",
        gsub("'", "''", fp)
      )
      one_schema <- DBI::dbGetQuery(con, one_sql)
      "read_position" %in% names(one_schema)
    }, logical(1)))
  }
  has_read_position <- isTRUE(all_have_read_position)

  wanted_sql <- paste(
    c(
      "read_id",
      "chrom",
      "CAST(start AS INTEGER) AS start",
      "CAST(\"end\" AS INTEGER) AS \"end\"",
      if (has_read_position) "read_position" else NULL,
      "query_kmer",
      "call_code",
      "read_length",
      "call_prob",
      "base_qual",
      "flag"
    ),
    collapse = ", "
  )

  # --- create calls table schema once, then append file batches -------------------
  create_calls_sql <- glue::glue("
    CREATE TABLE calls AS
    WITH src AS (
      SELECT *
      FROM read_parquet({file_list_sql}, filename = TRUE, union_by_name = TRUE)
      LIMIT 0
    ),
    tagged AS (
      SELECT
        m.sample_name AS sample_name,
        {wanted_sql}
      FROM src s
      LEFT JOIN file_map m
        ON m.file = s.filename
        OR m.base = REGEXP_REPLACE(s.filename, '^.*[/\\\\\\\\]', '')
    )
    SELECT *
    FROM tagged
    WHERE 1=0
  ")
  DBI::dbExecute(con, create_calls_sql)

  for (i in seq.int(1, nrow(df_files), by = file_batch_size)) {
    j <- min(i + file_batch_size - 1, nrow(df_files))
    batch_files   <- df_files$file[i:j]
    batch_list_sql <- paste0("['", paste(esc(batch_files), collapse = "','"), "']")

    insert_sql <- glue::glue("
      INSERT INTO calls
      WITH src AS (
        SELECT *
        FROM read_parquet({batch_list_sql}, filename = TRUE, union_by_name = TRUE)
      ),
      tagged AS (
        SELECT
          m.sample_name AS sample_name,
          {wanted_sql}
        FROM src s
        LEFT JOIN file_map m
          ON m.file = s.filename
          OR m.base = REGEXP_REPLACE(s.filename, '^.*[/\\\\]', '')
      )
      SELECT *
      FROM tagged
      {where_clause}
    ")
    DBI::dbExecute(con, insert_sql)
  }

  # --- capture row counts before closing ------------------------------------------
  sample_counts <- DBI::dbGetQuery(
    con,
    "SELECT sample_name, COUNT(*) AS n FROM calls GROUP BY sample_name ORDER BY sample_name"
  )

  # --- finish --------------------------------------------------------------------
  total_seconds <- as.numeric(Sys.time() - start_time, units = "secs")
  if (total_seconds > 60) {
    message("Database created at ", db_name,
            "\nTime elapsed: ", round(total_seconds / 60, 2), " minutes\n")
  } else {
    message("Database created at ", db_name,
            "\nTime elapsed: ", round(total_seconds, 2), " seconds\n")
  }

  # on.exit closes con — build the disconnected skeleton to return
  mod_db <- .make_mod_db_skeleton(db_name,
                                   list(memory_limit = memory_limit,
                                        temp_dir     = temp_dir,
                                        threads      = threads))
  mod_db$current_table <- "calls"
  mod_db$last_result   <- sample_counts
  invisible(mod_db)
}
