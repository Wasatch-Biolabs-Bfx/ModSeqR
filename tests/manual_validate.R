## Manual validation of 1.3.0 additions: in-DB tests vs R references, and
## parallel == serial for R-backed tests. Run with devtools::load_all() first.
suppressMessages({library(ModSeqR); library(DBI); library(duckdb); library(dplyr)})
set.seed(7)

# --- synthetic per-sample window data --------------------------------------
n_case <- 8; n_ctrl <- 10
cases    <- paste0("case",  seq_len(n_case))
controls <- paste0("ctrl",  seq_len(n_ctrl))
n_loci   <- 200
mk_locus <- function(i) {
  # real effect on ~1/4 of loci
  mu_c <- runif(1, 0.2, 0.8)
  mu_t <- if (i %% 4 == 0) min(max(mu_c + sample(c(-1,1),1)*runif(1,0.2,0.4),0.02),0.98) else mu_c
  depth <- function() pmax(rpois(1, 6), 1)
  dc <- sapply(seq_len(n_case), function(.) depth())
  dt <- sapply(seq_len(n_ctrl), function(.) depth())
  data.frame(
    sample_name = c(cases, controls),
    chrom = paste0("chr", 1 + (i %% 3)),
    start = i * 100L, end = i * 100L + 1L, num_sites = 5L,
    num_calls = c(dc, dt),
    m_counts  = c(rbinom(n_case, dc, mu_c), rbinom(n_ctrl, dt, mu_t))
  )
}
dat <- do.call(rbind, lapply(seq_len(n_loci), mk_locus))

dbf <- tempfile(fileext = ".mod.db")
con0 <- dbConnect(duckdb::duckdb(dbf))
dbWriteTable(con0, "windows", dat)
dbDisconnect(con0, shutdown = TRUE)

run <- function(calc, ncores = 1L) {
  db <- connect_mod_db(dbf)
  on.exit(disconnect_mod_db(db))
  db <- calc_mod_diff(db, input_table = "windows", output_table = "d",
                      cases = cases, controls = controls, mod_type = "m",
                      calc_type = calc, min_samples = 2L, n_cores = ncores,
                      overwrite = TRUE)
  get_mod_table(db, "d") |> arrange(chrom, start)
}

# --- 1. parallel == serial (beta_bin, fast_fisher) -------------------------
for (calc in c("beta_bin", "fast_fisher", "log_reg")) {
  s <- run(calc, 1L); p <- run(calc, 3L)
  key <- c("chrom","start")
  s <- s[do.call(order, s[key]), ]; p <- p[do.call(order, p[key]), ]
  pv_equal <- isTRUE(all.equal(s$p_val, p$p_val, tolerance = 1e-9)) &&
              isTRUE(all.equal(s$p_adjust, p$p_adjust, tolerance = 1e-9)) &&
              nrow(s) == nrow(p)
  cat(sprintf("[parallel==serial] %-12s rows s/p=%d/%d  p_val&p_adjust identical: %s\n",
              calc, nrow(s), nrow(p), pv_equal))
}

# --- 2. in-DB welch_t vs R t.test ------------------------------------------
w <- run("welch_t")
ref_welch <- function(row) {
  d <- dat[dat$chrom==row$chrom & dat$start==row$start, ]
  fc <- d$m_counts[d$sample_name %in% cases]    / d$num_calls[d$sample_name %in% cases]
  ft <- d$m_counts[d$sample_name %in% controls] / d$num_calls[d$sample_name %in% controls]
  tt <- tryCatch(t.test(fc, ft, var.equal = FALSE), error = function(e) NULL)
  if (is.null(tt)) NA_real_ else tt$p.value
}
w$p_ref <- vapply(seq_len(nrow(w)), function(i) ref_welch(w[i,]), numeric(1))
ok <- complete.cases(w$p_val, w$p_ref)
cat(sprintf("[welch_t vs t.test] n=%d  cor=%.4f  max|dp|=%.4f  median|dp|=%.4f\n",
            sum(ok), cor(w$p_val[ok], w$p_ref[ok]),
            max(abs(w$p_val[ok]-w$p_ref[ok])), median(abs(w$p_val[ok]-w$p_ref[ok]))))

# --- 3. in-DB prop_z vs R chisq.test (pooled, correct=FALSE) ---------------
z <- run("prop_z")
ref_chisq <- function(row) {
  d <- dat[dat$chrom==row$chrom & dat$start==row$start, ]
  xc <- sum(d$m_counts[d$sample_name %in% cases]);    nc <- sum(d$num_calls[d$sample_name %in% cases])
  xt <- sum(d$m_counts[d$sample_name %in% controls]); nt <- sum(d$num_calls[d$sample_name %in% controls])
  m <- matrix(c(xc, nc-xc, xt, nt-xt), 2)
  tryCatch(chisq.test(m, correct = FALSE)$p.value, error = function(e) NA_real_)
}
z$p_ref <- vapply(seq_len(nrow(z)), function(i) ref_chisq(z[i,]), numeric(1))
ok <- complete.cases(z$p_val, z$p_ref)
cat(sprintf("[prop_z vs chisq]   n=%d  cor=%.5f  max|dp|=%.5f\n",
            sum(ok), cor(z$p_val[ok], z$p_ref[ok]), max(abs(z$p_val[ok]-z$p_ref[ok]))))

# --- 4. quasi_bin sanity: phi>=1, p in [0,1], correlates with beta_bin -----
q <- run("quasi_bin"); b <- run("beta_bin")
m <- merge(q[,c("chrom","start","p_val")], b[,c("chrom","start","p_val")],
           by=c("chrom","start"), suffixes=c("_q","_b"))
ok <- complete.cases(m$p_val_q, m$p_val_b)
cat(sprintf("[quasi_bin] phi range=[%.2f,%.2f]  p in [0,1]: %s  cor(p_quasi,p_betabin)=%.3f (n=%d)\n",
            min(q$overdispersion,na.rm=TRUE), max(q$overdispersion,na.rm=TRUE),
            all(q$p_val>=0 & q$p_val<=1, na.rm=TRUE),
            cor(m$p_val_q[ok], m$p_val_b[ok]), sum(ok)))

cat("DONE\n")
