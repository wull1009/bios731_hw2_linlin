## source/ci_wald.R

fit0 <- lm(y ~ x + ., data = dat)

coef_tab <- summary(fit0)$coefficients

if (!("x" %in% rownames(coef_tab))) {
  ## x 在该样本里不可估计（例如 x 全 0/全 1 或共线），返回 NA
  theta_hat <- NA_real_
  se_hat    <- NA_real_
  CI_wald   <- c(NA_real_, NA_real_)
} else {
  theta_hat <- unname(coef(fit0)["x"])
  se_hat    <- coef_tab["x", "Std. Error"]
  
  z <- qnorm(1 - alpha/2)
  CI_wald <- c(
    lower = theta_hat - z * se_hat,
    upper = theta_hat + z * se_hat
  )
}
