# Beta-Binomial Differential Modification Test

Internal function called by
[`calc_mod_diff()`](https://wasatch-biolabs-bfx.github.io/ModSeqR/reference/calc_mod_diff.md)
when `calc_type = "beta_bin"`. Implements a beta-binomial model that
accounts for biological variability (overdispersion) across replicates,
making it appropriate for small-sample designs (2-4 replicates per
group) where the Wilcoxon test lacks power and the pooled Fisher test
ignores replicate variability.

## Usage

``` r
.bb_loglik(x, n, mu, phi)
```

## Arguments

- in_dat:

  A lazy `tbl` (DuckDB-backed) with columns: `sample_name`, grouping
  columns (e.g. `chrom`, `start`, `end`, `region_name`), `num_calls`,
  `mod_counts`, and `exp_group` (`"case"` or `"control"`).

## Value

A data.frame with one row per locus containing:

- grouping columns:

  e.g. chrom, start, end, region_name

- num_samples_case / num_samples_control:

  sample counts per group

- num_calls_case / num_calls_control:

  total reads per group

- mod_counts_case / mod_counts_control:

  total modified reads per group

- mod_frac_case / mod_frac_control:

  mean per-sample mod fraction

- meth_diff:

  mod_frac_case - mod_frac_control

- overdispersion:

  estimated phi

- p_val:

  likelihood-ratio test p-value

## Details

The beta-binomial log-likelihood for a single observation is: \$\$\log
P(x \| n, \mu, \phi) = \log\binom{n}{x} + \sum\_{j=0}^{x-1}
\log(\mu(1-\phi)/\phi + j) + \sum\_{j=0}^{n-x-1}
\log((1-\mu)(1-\phi)/\phi + j) - \sum\_{j=0}^{n-1} \log((1-\phi)/\phi +
j)\$\$

When \\\phi \to 0\\, the beta-binomial reduces to a standard binomial.
Optimisation uses `stats::optim(method = "L-BFGS-B")` with logit/log
transforms for numerical stability.

No additional package dependencies are required beyond base R.

## Model

At each genomic locus, each sample's modified-read count is modelled as:
\$\$X_i \sim \mathrm{BetaBinomial}(n_i,\\ \alpha,\\ \beta)\$\$ where
\\n_i\\ is the total read count for sample \\i\\, and \\\alpha, \beta\\
are the beta distribution shape parameters shared across samples within
a group.

We reparameterise to \\\mu = \alpha / (\alpha + \beta)\\ (mean
modification fraction) and \\\phi = 1 / (\alpha + \beta + 1)\\
(overdispersion, 0-1).

A likelihood-ratio test compares:

- **H0 (null):** one shared \\\mu\\ for both groups

- **H1 (alt):** separate \\\mu\_{case}\\ and \\\mu\_{control}\\

Both models share a single overdispersion parameter \\\phi\\. The test
statistic \\-2 \log(\Lambda)\\ is compared to a \\\chi^2_1\\
distribution to obtain a p-value.
