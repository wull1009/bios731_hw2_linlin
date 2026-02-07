## source/ci_boot_t.R

n <- nrow(dat)
K <- B_inner

boot_theta <- rep(NA_real_, B)
boot_se    <- rep(NA_real_, B)
t_star     <- rep(NA_real_, B)

for (b in 1:B) {
  
  idx_outer <- sample.int(n, n, replace = TRUE)
  dat_star  <- dat[idx_outer, , drop = FALSE]
  
  fit_star <- lm(y ~ x + ., data = dat_star)
  boot_theta[b] <- coef(fit_star)["x"]
  
  theta_inner <- rep(NA_real_, K)
  
  for (k in 1:K) {
    idx_inner <- sample.int(n, n, replace = TRUE)
    dat_star2 <- dat_star[idx_inner, , drop = FALSE]
    
    fit_star2 <- lm(y ~ x + ., data = dat_star2)
    theta_inner[k] <- coef(fit_star2)["x"]
  }
  
  boot_se[b] <- sd(theta_inner, na.rm = TRUE)
  
  t_star[b] <- (boot_theta[b] - theta_hat) / boot_se[b]
}
 
t_star <- t_star[is.finite(t_star)]

t_lo <- as.numeric(quantile(t_star, probs = alpha/2,     na.rm = TRUE))
t_hi <- as.numeric(quantile(t_star, probs = 1 - alpha/2, na.rm = TRUE))

CI_boot_t <- c(
  lower = theta_hat - t_hi * se_hat,
  upper = theta_hat - t_lo * se_hat
)
