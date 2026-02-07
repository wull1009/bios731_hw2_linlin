## source/ci_percentile.R

n <- nrow(dat)

boot_theta_pct <- rep(NA_real_, B)

for (b in 1:B) {
  idx <- sample.int(n, n, replace = TRUE)
  dat_star <- dat[idx, , drop = FALSE]
  
  fit_star <- lm(y ~ x + ., data = dat_star)
  boot_theta_pct[b] <- coef(fit_star)["x"]
}

qs <- as.numeric(quantile(boot_theta_pct,
                          probs = c(alpha/2, 1 - alpha/2),
                          na.rm = TRUE))
CI_pct <- c(lower = qs[1], upper = qs[2])

## bootstrap SE of theta_hat (top-level bootstrap)
se_boot_pct <- sd(boot_theta_pct, na.rm = TRUE)
