## source/ci_wald.R

fit0 <- lm(y ~ x + ., data = dat)
theta_hat <- coef(fit0)["x"]
se_hat    <- summary(fit0)$coefficients["x", "Std. Error"]

z <- qnorm(1 - alpha/2)

CI_wald <- c(
  lower = theta_hat - z * se_hat,
  upper = theta_hat + z * se_hat
)
