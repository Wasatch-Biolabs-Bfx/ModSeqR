#' Calculate Differential Methylation
#'
#' This function calculates differential methylation between specified case and control groups using various statistical methods. 
#' The results are stored in a DuckDB database for further analysis.
#'
#' @param mod_db A list containing the database file path. This should be a valid "mod_db" class object.
#' @param call_type A string representing the name of the table in the database from which to pull the data. 
#' Default is "positions".
#' @param output_table Destination table name for results. If NULL, defaults to paste0("mod_diff_", call_type).
#' @param cases A character vector containing the sample names for the case group.
#' @param controls A character vector containing the sample names for the control group.
#' @param mod_type A string indicating the type of modification to analyze. 
#' Default is "mh" for methylation/hydroxymethylation. Other codes include
#'   "a" for 6mA, "17596" for inosine, and "17802" for pseudouridine.
#'   Bare numeric codes are automatically prefixed with "m_".
#' @param calc_type A string specifying the statistical method to use.
#'   Options: "wilcox", "beta_bin", "fast_fisher", "r_fisher", "log_reg".
#'   Default is NULL, in which case:
#'   \itemize{
#'     \item "wilcox" if both groups have >= 5 samples
#'     \item "fast_fisher" if either group has fewer than 5 samples
#'   }
#' @param temp_dir Directory for DuckDB temporary files (default \code{tempdir()}).
#' @param threads Integer DuckDB thread count. If \code{NULL}, an internal heuristic
#'   (typically all-but-one core) is used.
#' @param memory_limit DuckDB memory limit string (e.g. \code{"16384MB"}).
#'   If \code{NULL}, an internal heuristic (~80\% of RAM) is used.
#' @param min_sites Minimum number of distinct modification sites (e.g., CpGs)
#'   required per sample within a window. Windows where any sample has fewer
#'   than this many sites with calls are dropped before testing. This filters
#'   out windows with poor breadth of coverage. Only applies when the input
#'   table contains a \code{num_sites} column (i.e., windows).
#'   Default is \code{NULL} (no filtering).
#' @param overwrite If TRUE and output_table exists, it is dropped before writing.
#'
#' @details
#' The function connects to the specified DuckDB database and retrieves methylation data from the specified call type table. 
#' It summarizes the data for cases and controls, calculates p-values based on the specified method, and stores the results in the 
#' "meth_diff" table. Resource pragmas (\code{temp_directory}, \code{threads},
#' \code{memory_limit}) are set via internal heuristics unless overridden.
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
#'  # Get methylation statistics for the 'positions' call type without plotting
#'  calc_mod_diff(mod_db = mod_db, 
#'                call_type = "positions",
#'                cases = c("Blood1_chr21", "Blood2_chr21", "Blood3_chr21"),
#'                controls = c("Sperm1_chr21", "Sperm2_chr21", "Sperm3_chr21")))
#' }
#'                
#' @importFrom DBI dbConnect dbDisconnect dbExistsTable dbRemoveTable dbExecute dbWriteTable
#' @importFrom duckdb duckdb
#' @importFrom dplyr tbl select any_of mutate case_when filter pull summarize inner_join join_by rename_with collect arrange
#' @importFrom tidyr pivot_wider
#' @importFrom stats fisher.test p.adjust dhyper phyper glm.fit pchisq optim plogis qlogis var
#'
#' @export

calc_mod_diff <- function(mod_db,
                          call_type = "positions",
                          output_table = NULL,
                          cases,
                          controls,
                          mod_type = "mh",
                          calc_type = NULL,
                          temp_dir = tempdir(),
                          threads = NULL,
                          memory_limit = NULL,
                          min_sites = NULL, 
                          overwrite = TRUE)
{
  start_time <- Sys.time()

  # Open the database connection
  mod_db <- .modhelper_connectDB(mod_db)

  # Resource caps
  caps <- .auto_duckdb_resource_caps(0.80)
  thr  <- if (is.null(threads)) caps$threads else threads
  mem  <- if (is.null(memory_limit)) caps$memory_limit else memory_limit

  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  DBI::dbExecute(mod_db$con, sprintf("PRAGMA temp_directory='%s';", temp_dir))
  DBI::dbExecute(mod_db$con, sprintf("PRAGMA memory_limit='%s';", mem))
  DBI::dbExecute(mod_db$con, sprintf("PRAGMA threads=%d;", thr))

  # check for windows function
  if (!dbExistsTable(mod_db$con, call_type)) { # add db_con into object and put in every function...
    stop(call_type, " table does not exist. Build it with summarize_mod_positions(), ",
         "summarize_mod_regions(), or summarize_mod_windows().")
  }
  
  # Discover available *_counts columns and validate mod_type
  cols <- colnames(dplyr::tbl(mod_db$con, call_type))
  counts_cols <- grep("_counts$", cols, value = TRUE)
  available_labels <- sub("_counts$", "", counts_cols)
  
  mod_type <- mod_type[1]
  
  # Allow bare numeric codes: e.g. 17596 -> "m_17596"
  if (grepl("^\\d+$", mod_type)) {
    mod_type <- paste0("m_", mod_type)
  }
  
  mod_counts_col <- paste0(mod_type, "_counts")
  if (!mod_counts_col %in% cols) {
    stop("Requested mod_type '", mod_type, "' not found in ", call_type, ". ",
         "Available mod types are: ",
         paste(sort(available_labels), collapse = ", "), ".\n",
         "Tip: rebuild ", call_type, " with summarize_* and include mod_code = '", mod_type, "'.")
  }
  if (!"num_calls" %in% cols) {
    stop(call_type, " is missing required column 'num_calls'. ",
         "Recreate it with the latest summarize_* function.")
  }
  
  if (is.null(output_table) || !nzchar(output_table)) {
    mod_diff_table <- paste0("mod_diff_", call_type)
  } else {
    mod_diff_table <- output_table
  }
  
  if (DBI::dbExistsTable(mod_db$con, mod_diff_table)) {
    if (overwrite) {
      DBI::dbRemoveTable(mod_db$con, mod_diff_table)
    } else {
      stop("Output table '", mod_diff_table, "' already exists. Set overwrite = TRUE or choose a different output_table.")
    }
  }
  
  in_dat <-
    dplyr::tbl(mod_db$con, call_type) |>
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
      warning("min_sites was set but '", call_type,
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
  
  # Compute p-values / diffs
  result <- switch(calc_type,
                   wilcox      = .calc_diff_wilcox(in_dat),
                   beta_bin    = .calc_diff_betabin(in_dat),
                   fast_fisher = .calc_diff_fisher(in_dat, calc_type = "fast_fisher"),
                   r_fisher    = .calc_diff_fisher(in_dat, calc_type = "r_fisher"),
                   log_reg     = .calc_diff_logreg(in_dat),
                   stop("Unknown calc_type: ", calc_type,
                        ". Use 'beta_bin', 'fast_fisher', 'r_fisher', 'wilcox', or 'log_reg'.")
  ) |>
    dplyr::rename_with(~ gsub("^mod", mod_type, .x))
  
  # Build your final table...
  result |>
    dplyr::collect() |>
    dplyr::mutate(
      p_adjust = stats::p.adjust(p_val, method = "BH"),
      p_val    = pmax(p_val, .Machine$double.xmin),
      p_adjust = pmax(p_adjust, .Machine$double.xmin)) |>
    dplyr::arrange(p_adjust) |>
    DBI::dbWriteTable(conn = mod_db$con, 
                      name = mod_diff_table, 
                      append = TRUE)

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
  
  if (call_type == "windows") {
    message("Call collapse_mod_windows() to collapse significant windows.\n")
  } 
  
  # Print a preview of what table looks like
  result_head <- dplyr::tbl(mod_db$con, mod_diff_table) |> head() |> dplyr::collect()
  print(result_head)

  mod_db$current_table <- mod_diff_table
  mod_db$last_result   <- result_head
  mod_db <- .modhelper_cleanup(mod_db)
  invisible(mod_db)
}



## Calculate p-values using Wilcoxon rank-sum test on per-sample methylation fractions.
## This compares the distribution of m_frac between case and control samples per region/window.
.calc_diff_wilcox <- function(in_dat)
{
  # Work at the per-sample level: compute methylation fraction for each sample x region
  frac_dat <-
    in_dat |>
    dplyr::mutate(
      mod_frac = dplyr::if_else(
        num_calls > 0,
        mod_counts / num_calls,
        NA_real_
      )
    ) |>
    dplyr::collect()
  
  # Figure out which columns define the genomic unit (region/window/position)
  group_vars <- setdiff(
    colnames(frac_dat),
    c("sample_name", "exp_group", "num_calls", "mod_counts", "mod_frac", 
      "num_sites")
  )
  
  # Summarize per region/window and run Wilcoxon tests
  frac_dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      num_samples_case    = sum(exp_group == "case"),
      num_samples_control = sum(exp_group == "control"),
      
      num_calls_case      = sum(num_calls[exp_group == "case"],    na.rm = TRUE),
      num_calls_control   = sum(num_calls[exp_group == "control"], na.rm = TRUE),
      
      mod_counts_case     = sum(mod_counts[exp_group == "case"],    na.rm = TRUE),
      mod_counts_control  = sum(mod_counts[exp_group == "control"], na.rm = TRUE),
      
      mod_frac_case = mean(mod_frac[exp_group == "case"],    na.rm = TRUE),
      mod_frac_control = mean(mod_frac[exp_group == "control"], na.rm = TRUE),
      
      meth_diff = mean(mod_frac[exp_group == "case"],    na.rm = TRUE) -
        mean(mod_frac[exp_group == "control"], na.rm = TRUE),
      
      p_val = {
        case_vals <- mod_frac[exp_group == "case"]
        ctrl_vals <- mod_frac[exp_group == "control"]
        
        case_vals <- case_vals[is.finite(case_vals)]
        ctrl_vals <- ctrl_vals[is.finite(ctrl_vals)]
        
        if (length(case_vals) > 0 && length(ctrl_vals) > 0) {
          suppressWarnings(
            stats::wilcox.test(case_vals, ctrl_vals)$p.value
          )
        } else {
          NA_real_
        }
      },
      
      .groups = "drop"
    )
}


## Calculate p-values using fisher exact tests. If there are multiple samples,
## they will be combined.
.calc_diff_fisher <- function(in_dat,
                              calc_type)
{
  # Combine replicates and pivot wider
  dat <-
    in_dat |>
    dplyr::summarize(
      .by = c(exp_group, any_of(c("region_name", "chrom", "start", "end"))),
      num_calls = sum(num_calls, na.rm = TRUE),
      mod_counts = sum(mod_counts, na.rm = TRUE)) |>
    dplyr::mutate(
      c_counts = num_calls - mod_counts) |>
    pivot_wider(
      names_from = exp_group,
      values_from = c(num_calls, mod_counts, c_counts),
      values_fill = 0)
  
  # Extract matrix and calculate p-vals
  pvals <-
    dat |>
    dplyr::select(!any_of(c("region_name", "chrom", "start", "end"))) |>
    distinct() |>
    dplyr::mutate(
      mod_frac_case = mod_counts_case /
        (num_calls_case),
      mod_frac_control = mod_counts_control /
        (num_calls_control),
      meth_diff = mod_counts_case /
        (num_calls_case) -
        mod_counts_control /
        (num_calls_control)) |>
    collect() |>
    dplyr::mutate(
      p_val = switch(
        calc_type,
        fast_fisher = .fast_fisher(
          q = mod_counts_case,
          m = mod_counts_case + mod_counts_control,
          n = c_counts_case + c_counts_control,
          k = num_calls_case),
        r_fisher = .r_fisher(
          a = mod_counts_control,
          b = mod_counts_case,
          c = c_counts_control,
          d = c_counts_case)))
  
  dat |>
    inner_join(
      pvals,
      by = join_by(num_calls_case, num_calls_control,
                   mod_counts_case, mod_counts_control,
                   c_counts_case,   c_counts_control),
      copy = TRUE)
  
  
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


.calc_diff_logreg <- function(in_dat)
{
  pvals <-
    in_dat |>
    mutate(
      cov = num_calls + mod_counts,
      mod_frac = mod_counts / cov) |>
    collect() |>
    summarize(
      .by = c(chrom, start),
      mean_cov = mean(cov, na.rm = TRUE),
      mean_frac_case = mean(mod_frac[exp_group == "case"]),
      mean_frac_ctrl = mean(mod_frac[exp_group == "control"]),
      mean_diff = mean_frac_case - mean_frac_ctrl,
      p_val = .logreg(mod_frac, cov, exp_group))
  
  # Pivot wider, add pvals, and return
  in_dat |>
    pivot_wider(
      id_cols = c(chrom, ref_position),
      names_from = sample_name,
      values_from = c(num_calls, mod_counts),
      values_fill = 0) |>
    inner_join(
      pvals, by = join_by(chrom, ref_position),
      copy = TRUE)
}


.logreg <- function(mod_frac,
                    cov,
                    exp_group)
{
  exp_group <- as.numeric(factor(exp_group))
  fit <- glm.fit(exp_group, mod_frac,
                 weights = cov / sum(cov), family = binomial())
  deviance <- fit$null.deviance - fit$deviance
  
  pchisq(deviance, 1, lower.tail = FALSE)
}