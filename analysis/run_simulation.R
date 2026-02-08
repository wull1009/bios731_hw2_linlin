## analysis/run_simulation.R

library(here)

## ---- reproducible RNG across parallel jobs ----
RNGkind("L'Ecuyer-CMRG")
set.seed(2024)

## ---- read SLURM array task id ----
args <- commandArgs(trailingOnly = TRUE)
JOBID <- as.integer(args[1])
if (!is.finite(JOBID)) stop("JOBID must be an integer.")

## ---- load data generator ----
source(here("source", "generate_data.R"))

## ---- simulation parameters ----
alpha   <- 0.05
n_sim   <- 475      
B       <- 500
B_inner <- 100

## ---- scenario grid (18 scenarios) ----
n_vec    <- c(10, 50, 500)
beta_vec <- c(0, 0.5, 2)
err_vec  <- c("normal", "t3")

scenarios <- expand.grid(
  n = n_vec,
  beta_treat = beta_vec,
  error_type = err_vec,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
scenarios$scenario_id <- seq_len(nrow(scenarios))

if (JOBID < 1 || JOBID > nrow(scenarios)) {
  stop(sprintf("JOBID must be in 1..%d, got %d", nrow(scenarios), JOBID))
}

## ---- output directory ----
out_dir <- here("data", "scenarios")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## ---- pick ONE scenario based on JOBID ----
s <- JOBID

n_s   <- scenarios$n[s]
bt_s  <- scenarios$beta_treat[s]
err_s <- scenarios$error_type[s]
sid   <- scenarios$scenario_id[s]
truth <- bt_s

message(sprintf("Running scenario %d: n=%s, beta_treat=%s, error_type=%s",
                sid, n_s, bt_s, err_s))

## ---- containers ----
theta_hat_vec <- rep(NA_real_, n_sim)
se_hat_vec    <- rep(NA_real_, n_sim)

se_boot_pct_vec <- rep(NA_real_, n_sim)

wald_lo <- rep(NA_real_, n_sim); wald_hi <- rep(NA_real_, n_sim)
pct_lo  <- rep(NA_real_, n_sim); pct_hi  <- rep(NA_real_, n_sim)
bt_lo   <- rep(NA_real_, n_sim); bt_hi   <- rep(NA_real_, n_sim)

time_wald <- rep(NA_real_, n_sim)
time_pct  <- rep(NA_real_, n_sim)
time_bt   <- rep(NA_real_, n_sim)

## ---- main simulation loop (replicates) ----
for (r in seq_len(n_sim)) {
  
  dat <- generate_data(n = n_s, beta_treat = bt_s, error_type = err_s)
  
  ## Wald (defines theta_hat, se_hat, CI_wald)
  t0 <- proc.time()[3]
  source(here("source", "ci_wald.R"), local = TRUE)
  time_wald[r] <- proc.time()[3] - t0
  
  theta_hat_vec[r] <- theta_hat
  se_hat_vec[r]    <- se_hat
  
  wald_lo[r] <- CI_wald[1]
  wald_hi[r] <- CI_wald[2]
  
  ## Bootstrap percentile (defines CI_pct, se_boot_pct)
  t0 <- proc.time()[3]
  source(here("source", "ci_percentile.R"), local = TRUE)
  time_pct[r] <- proc.time()[3] - t0
  
  pct_lo[r] <- CI_pct[1]
  pct_hi[r] <- CI_pct[2]
  se_boot_pct_vec[r] <- se_boot_pct
  
  ## Bootstrap-t (defines CI_boot_t; uses theta_hat, se_hat from Wald step)
  t0 <- proc.time()[3]
  source(here("source", "ci_boot_t.R"), local = TRUE)
  time_bt[r] <- proc.time()[3] - t0
  
  bt_lo[r] <- CI_boot_t[1]
  bt_hi[r] <- CI_boot_t[2]
}

## ---- performance summary ----
coverage <- function(lo, hi) mean(lo <= truth & truth <= hi, na.rm = TRUE)
mean_width <- function(lo, hi) mean(hi - lo, na.rm = TRUE)

out <- list(
  scenario = scenarios[s, ],
  params   = list(alpha = alpha, n_sim = n_sim, B = B, B_inner = B_inner),
  truth    = truth,
  perf     = list(
    wald = list(
      coverage   = coverage(wald_lo, wald_hi),
      mean_width = mean_width(wald_lo, wald_hi),
      mean_time  = mean(time_wald, na.rm = TRUE)
    ),
    boot_percentile = list(
      coverage   = coverage(pct_lo, pct_hi),
      mean_width = mean_width(pct_lo, pct_hi),
      mean_time  = mean(time_pct, na.rm = TRUE)
    ),
    boot_t = list(
      coverage   = coverage(bt_lo, bt_hi),
      mean_width = mean_width(bt_lo, bt_hi),
      mean_time  = mean(time_bt, na.rm = TRUE)
    )
  ),
  reps = list(
    theta_hat      = theta_hat_vec,
    se_hat         = se_hat_vec,
    se_boot_pct    = se_boot_pct_vec,
    wald           = cbind(lower = wald_lo, upper = wald_hi),
    boot_percentile= cbind(lower = pct_lo,  upper = pct_hi),
    boot_t         = cbind(lower = bt_lo,   upper = bt_hi),
    time           = cbind(wald = time_wald, boot_percentile = time_pct, boot_t = time_bt)
  )
)

fname <- sprintf("scenario_%02d_n%s_bt%s_err%s.rds",
                 sid, n_s, gsub("\\.", "p", as.character(bt_s)), err_s)

saveRDS(out, file = file.path(out_dir, fname))
message("Saved: ", file.path(out_dir, fname))
message("DONE.")
