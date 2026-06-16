## In-DuckDB closed-form differential tests.
##
## These calc types compute their per-locus statistic entirely in SQL (no R is
## needed for the statistic itself), like `wilcox`. They are aggregation-based
## (GROUP BY locus), so unlike the rank-sum they need no global sort and run as a
## single CREATE TABLE AS in calc_mod_diff(). All share the Abramowitz & Stegun
## 26.2.17 normal-CDF approximation (max error ~7.5e-8) for the two-sided p-value.
##
## Intended primarily as fast screens: `welch_t` and `quasi_bin` are per-sample
## (no pseudoreplication), `prop_z` is pooled (fast, but shares Fisher's
## read-as-observation caveat). For final small-sample inference the exact
## `beta_bin` LRT remains the reference; `quasi_bin` approximates it in closed
## form (quasi-likelihood / Rao-Scott overdispersion correction).


## SQL expression for the two-sided normal-approx p-value of a z expression.
## `z` is a SQL expression string; returns a self-contained SQL expression.
## NULL z (undefined statistic) propagates to NULL p. Uses A&S 26.2.17.
.norm_p2_sql <- function(z)
{
  z  <- sprintf("(%s)", z)
  t1 <- sprintf("(1.0 / (1.0 + 0.2316419 * ABS(%s)))", z)
  sprintf(
    "CASE WHEN %s IS NULL THEN NULL ELSE LEAST(
        2.0 * ((1.0 / SQRT(2.0 * PI())) * EXP(-POWER(ABS(%s), 2) / 2.0)) * (
            0.319381530 * %s
          - 0.356563782 * POWER(%s, 2)
          + 1.781477937 * POWER(%s, 3)
          - 1.821255978 * POWER(%s, 4)
          + 1.330274429 * POWER(%s, 5)
        ), 1.0) END",
    z, z, t1, t1, t1, t1, t1
  )
}

## Shared helpers for building the per-locus SQL.
.qi_fun <- function(con) function(nms) paste(
  vapply(nms, function(nm) as.character(DBI::dbQuoteIdentifier(con, nm)), character(1)),
  collapse = ", "
)

## Common per-group aggregates over per-sample fractions, returned as a `_grp`
## CTE body. Produces, per (locus, exp_group): _n_g (samples), _nc_g (calls),
## _mc_g (mod counts), _mf_avg (mean per-sample fraction), _mf_var (sample var).
## `src_sql` is the rendered source; `gvars_sql` the quoted group vars.
.frac_grp_ctes <- function(src_sql, gvars_sql)
{
  glue::glue("
    _frac AS (
      SELECT *, mod_counts * 1.0 / num_calls AS _mf
      FROM ({src_sql}) _s
      WHERE num_calls > 0 AND mod_counts IS NOT NULL
    ),
    _grp AS (
      SELECT
        {gvars_sql}, exp_group,
        COUNT(*)        AS _n_g,
        SUM(num_calls)  AS _nc_g,
        SUM(mod_counts) AS _mc_g,
        AVG(_mf)        AS _mf_avg,
        VAR_SAMP(_mf)   AS _mf_var
      FROM _frac
      GROUP BY {gvars_sql}, exp_group
    ),
    _locus AS (
      SELECT
        {gvars_sql},
        MAX(CASE WHEN exp_group = 'case'    THEN _n_g    END) AS num_samples_case,
        MAX(CASE WHEN exp_group = 'control' THEN _n_g    END) AS num_samples_control,
        MAX(CASE WHEN exp_group = 'case'    THEN _nc_g   END) AS num_calls_case,
        MAX(CASE WHEN exp_group = 'control' THEN _nc_g   END) AS num_calls_control,
        MAX(CASE WHEN exp_group = 'case'    THEN _mc_g   END) AS mod_counts_case,
        MAX(CASE WHEN exp_group = 'control' THEN _mc_g   END) AS mod_counts_control,
        MAX(CASE WHEN exp_group = 'case'    THEN _mf_avg END) AS mod_frac_case,
        MAX(CASE WHEN exp_group = 'control' THEN _mf_avg END) AS mod_frac_control,
        MAX(CASE WHEN exp_group = 'case'    THEN _mf_var END) AS _var_case,
        MAX(CASE WHEN exp_group = 'control' THEN _mf_var END) AS _var_control
      FROM _grp
      GROUP BY {gvars_sql}
    )"
  )
}

## Final projection columns shared by all in-DB tests (everything except p_val,
## which each test supplies). `gvars_sql` quoted group vars.
.locus_select_cols <- function(gvars_sql)
{
  glue::glue("
      {gvars_sql},
      num_samples_case,
      num_samples_control,
      num_calls_case,
      num_calls_control,
      mod_counts_case,
      mod_counts_control,
      mod_frac_case,
      mod_frac_control,
      mod_frac_case - mod_frac_control AS meth_diff")
}


## --- Welch's t-test on per-sample modification fractions --------------------
## Per-sample, unequal-variance. The t statistic is mapped to a normal deviate
## via the Wallace (1959) approximation, then the A&S normal CDF gives the
## two-sided p. Accurate enough for screening; the t->z map is approximate in
## the extreme small-df tail.
.calc_diff_welch_t <- function(in_dat)
{
  con        <- dbplyr::remote_con(in_dat)
  group_vars <- setdiff(colnames(in_dat),
                        c("sample_name", "exp_group", "num_calls", "mod_counts", "num_sites"))
  qi <- .qi_fun(con)
  src_sql   <- as.character(dbplyr::sql_render(in_dat))
  gvars_sql <- qi(group_vars)

  sql <- glue::glue("
    WITH
    {.frac_grp_ctes(src_sql, gvars_sql)},
    _t AS (
      SELECT *,
        _var_case    / NULLIF(num_samples_case, 0)    AS _se2_case,
        _var_control / NULLIF(num_samples_control, 0) AS _se2_control
      FROM _locus
    ),
    _stat AS (
      SELECT *,
        (_se2_case + _se2_control)                                   AS _se2,
        CASE WHEN (_se2_case + _se2_control) > 0
             THEN (mod_frac_case - mod_frac_control)
                  / SQRT(_se2_case + _se2_control) END               AS _tval,
        -- Welch-Satterthwaite degrees of freedom
        CASE WHEN (_se2_case + _se2_control) > 0
              AND num_samples_case > 1 AND num_samples_control > 1
             THEN POWER(_se2_case + _se2_control, 2) /
                  ( POWER(_se2_case, 2) / NULLIF(num_samples_case - 1, 0)
                  + POWER(_se2_control, 2) / NULLIF(num_samples_control - 1, 0) )
             END                                                     AS _df
      FROM _t
    ),
    _z AS (
      SELECT *,
        -- Wallace t->z normal approximation
        CASE WHEN _tval IS NULL OR _df IS NULL OR _df <= 0 THEN NULL
             ELSE SIGN(_tval) * (8.0 * _df + 1.0) / (8.0 * _df + 3.0)
                  * SQRT(_df * LN(1.0 + _tval * _tval / _df)) END    AS _z
      FROM _stat
    )
    SELECT
    {.locus_select_cols(gvars_sql)},
      {.norm_p2_sql('_z')} AS p_val
    FROM _z
  ")
  dplyr::tbl(con, dplyr::sql(sql))
}


## --- Pooled two-proportion z / chi-square test ------------------------------
## Pools all reads within each group into a single proportion. Fast, fully in
## DB. NOTE: pooled => treats reads as independent observations
## (pseudoreplication), same caveat as fast_fisher; use for speed/screening.
## chi-square(1 df) p-value equals the two-sided normal p of z = sqrt(chisq).
.calc_diff_prop_z <- function(in_dat)
{
  con        <- dbplyr::remote_con(in_dat)
  group_vars <- setdiff(colnames(in_dat),
                        c("sample_name", "exp_group", "num_calls", "mod_counts", "num_sites"))
  qi <- .qi_fun(con)
  src_sql   <- as.character(dbplyr::sql_render(in_dat))
  gvars_sql <- qi(group_vars)

  sql <- glue::glue("
    WITH
    {.frac_grp_ctes(src_sql, gvars_sql)},
    _z AS (
      SELECT *,
        (mod_counts_case + mod_counts_control) * 1.0
          / NULLIF(num_calls_case + num_calls_control, 0)            AS _p_pool,
        (mod_counts_case * 1.0 / NULLIF(num_calls_case, 0))          AS _p_case,
        (mod_counts_control * 1.0 / NULLIF(num_calls_control, 0))    AS _p_ctrl
      FROM _locus
    ),
    _z2 AS (
      SELECT *,
        CASE WHEN _p_pool IS NULL OR _p_pool <= 0 OR _p_pool >= 1
                  OR num_calls_case IS NULL OR num_calls_control IS NULL THEN NULL
             ELSE (_p_case - _p_ctrl) /
                  SQRT(_p_pool * (1.0 - _p_pool)
                       * (1.0 / num_calls_case + 1.0 / num_calls_control)) END AS _z
      FROM _z
    )
    SELECT
    {.locus_select_cols(gvars_sql)},
      {.norm_p2_sql('_z')} AS p_val
    FROM _z2
  ")
  dplyr::tbl(con, dplyr::sql(sql))
}


## --- Overdispersion-corrected proportion test (quasi-binomial) --------------
## Closed-form, fully-in-DB approximation to the beta-binomial: a pooled-count
## 2-proportion z is deflated by sqrt(phi_hat), where phi_hat is the Pearson
## quasi-likelihood dispersion estimated from between-sample variability around
## each group's mean. phi_hat is floored at 1 (no under-dispersion credit).
## This captures the beta-binomial's key feature (down-weighting for replicate
## variability) without per-locus optimisation. The statistic is a SCORE-type
## (Rao-Scott / quasi-likelihood) test -- the variance uses the pooled (null)
## proportion, not the unpooled (alternative) variance of a Wald test -- so it is
## robust near the 0/1 boundary. It approximates the beta-binomial LRT and differs
## from it most when phi_hat floors at 1 in small samples (then it reduces to the
## pooled score test and is anti-conservative) -- use beta_bin for final inference.
.calc_diff_quasi_bin <- function(in_dat)
{
  con        <- dbplyr::remote_con(in_dat)
  group_vars <- setdiff(colnames(in_dat),
                        c("sample_name", "exp_group", "num_calls", "mod_counts", "num_sites"))
  qi <- .qi_fun(con)
  src_sql   <- as.character(dbplyr::sql_render(in_dat))
  gvars_sql <- qi(group_vars)

  sql <- glue::glue("
    WITH
    _frac AS (
      SELECT *, mod_counts * 1.0 / num_calls AS _mf
      FROM ({src_sql}) _s
      WHERE num_calls > 0 AND mod_counts IS NOT NULL
    ),
    -- group MoM mean per locus, attached to each sample row
    _gm AS (
      SELECT *,
        SUM(mod_counts) OVER (PARTITION BY {gvars_sql}, exp_group) * 1.0
          / NULLIF(SUM(num_calls) OVER (PARTITION BY {gvars_sql}, exp_group), 0) AS _mu_g
      FROM _frac
    ),
    -- per-sample Pearson contribution to the dispersion estimate
    _resid AS (
      SELECT *,
        CASE WHEN _mu_g IS NULL OR _mu_g <= 0 OR _mu_g >= 1 THEN NULL
             ELSE POWER(mod_counts - num_calls * _mu_g, 2)
                  / (num_calls * _mu_g * (1.0 - _mu_g)) END AS _pearson
      FROM _gm
    ),
    _grp AS (
      SELECT
        {gvars_sql}, exp_group,
        COUNT(*)        AS _n_g,
        SUM(num_calls)  AS _nc_g,
        SUM(mod_counts) AS _mc_g,
        AVG(_mf)        AS _mf_avg
      FROM _resid
      GROUP BY {gvars_sql}, exp_group
    ),
    _disp AS (
      SELECT {gvars_sql},
        SUM(_pearson)              AS _pearson_sum,
        COUNT(_pearson)            AS _n_resid
      FROM _resid
      GROUP BY {gvars_sql}
    ),
    _locus AS (
      SELECT
        {gvars_sql},
        MAX(CASE WHEN exp_group = 'case'    THEN _n_g    END) AS num_samples_case,
        MAX(CASE WHEN exp_group = 'control' THEN _n_g    END) AS num_samples_control,
        MAX(CASE WHEN exp_group = 'case'    THEN _nc_g   END) AS num_calls_case,
        MAX(CASE WHEN exp_group = 'control' THEN _nc_g   END) AS num_calls_control,
        MAX(CASE WHEN exp_group = 'case'    THEN _mc_g   END) AS mod_counts_case,
        MAX(CASE WHEN exp_group = 'control' THEN _mc_g   END) AS mod_counts_control,
        MAX(CASE WHEN exp_group = 'case'    THEN _mf_avg END) AS mod_frac_case,
        MAX(CASE WHEN exp_group = 'control' THEN _mf_avg END) AS mod_frac_control
      FROM _grp
      GROUP BY {gvars_sql}
    ),
    _z AS (
      SELECT l.*,
        d._pearson_sum, d._n_resid,
        -- Pearson dispersion phi_hat = SS / (N_samples - 2), floored at 1
        GREATEST(
          d._pearson_sum / NULLIF(d._n_resid - 2, 0), 1.0)          AS _phi_hat,
        (mod_counts_case + mod_counts_control) * 1.0
          / NULLIF(num_calls_case + num_calls_control, 0)           AS _p_pool
      FROM _locus l
      LEFT JOIN _disp d USING ({gvars_sql})
    ),
    _zc AS (
      SELECT *,
        CASE WHEN _p_pool IS NULL OR _p_pool <= 0 OR _p_pool >= 1
                  OR num_calls_case IS NULL OR num_calls_control IS NULL THEN NULL
             ELSE ( (mod_counts_case * 1.0 / num_calls_case)
                  - (mod_counts_control * 1.0 / num_calls_control) )
                  / SQRT(_p_pool * (1.0 - _p_pool)
                         * (1.0 / num_calls_case + 1.0 / num_calls_control)
                         * COALESCE(_phi_hat, 1.0)) END             AS _z
      FROM _z
    )
    SELECT
    {.locus_select_cols(gvars_sql)},
      _phi_hat AS overdispersion,
      {.norm_p2_sql('_z')} AS p_val
    FROM _zc
  ")
  dplyr::tbl(con, dplyr::sql(sql))
}
