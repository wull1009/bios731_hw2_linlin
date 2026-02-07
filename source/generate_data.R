library(here)

generate_data <- function(n,
                          beta_treat,
                          error_type = c("normal", "t3")) {
  
  error_type <- match.arg(error_type)
  
  ## treatment
  x <- rbinom(n, size = 1, prob = 0.5)
  
  ## confounders
  z1 <- rnorm(n)
  z2 <- rnorm(n)
  Z  <- cbind(z1, z2)
  
  ## coefficients
  beta0 <- 0
  gamma <- c(1, -1)
  
  ## errors
  if (error_type == "normal") {
    eps <- rnorm(n, mean = 0, sd = sqrt(2))
  } else {
    # t3 scaled to Var = 2
    eps <- rt(n, df = 3) * sqrt(2 * (3 - 2) / 3)
  }
  
  ## outcome
  y <- beta0 + beta_treat * x + Z %*% gamma + eps
  
  dat <- data.frame(
    y  = as.numeric(y),
    x  = x,
    z1 = z1,
    z2 = z2
  )
  
  return(dat)
}
