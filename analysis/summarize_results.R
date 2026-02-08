## analysis/summarize_results.R

library(here)
library(ggplot2)

in_dir <- here("data", "scenarios")
out_dir <- here("results")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

files <- list.files(in_dir, pattern = "\\.rds$", full.names = TRUE)
stopifnot(length(files) > 0)

## ---- build summary table ----
rows <- list()

for (f in files) {
  obj <- readRDS(f)
  
  sc <- obj$scenario
  truth <- obj$truth
  
  rows[[length(rows) + 1]] <- data.frame(
    scenario_id = sc$scenario_id,
    n = sc$n,
    beta_treat = sc$beta_treat,
    error_type = sc$error_type,
    method = "wald",
    bias = mean(obj$reps$theta_hat - truth, na.rm = TRUE),
    coverage = obj$perf$wald$coverage,
    mean_width = obj$perf$wald$mean_width,
    mean_time = obj$perf$wald$mean_time,
    stringsAsFactors = FALSE
  )
  
  rows[[length(rows) + 1]] <- data.frame(
    scenario_id = sc$scenario_id,
    n = sc$n,
    beta_treat = sc$beta_treat,
    error_type = sc$error_type,
    method = "boot_percentile",
    bias = mean(obj$reps$theta_hat - truth, na.rm = TRUE),
    coverage = obj$perf$boot_percentile$coverage,
    mean_width = obj$perf$boot_percentile$mean_width,
    mean_time = obj$perf$boot_percentile$mean_time,
    stringsAsFactors = FALSE
  )
  
  rows[[length(rows) + 1]] <- data.frame(
    scenario_id = sc$scenario_id,
    n = sc$n,
    beta_treat = sc$beta_treat,
    error_type = sc$error_type,
    method = "boot_t",
    bias = mean(obj$reps$theta_hat - truth, na.rm = TRUE),
    coverage = obj$perf$boot_t$coverage,
    mean_width = obj$perf$boot_t$mean_width,
    mean_time = obj$perf$boot_t$mean_time,
    stringsAsFactors = FALSE
  )
}

summary_df <- do.call(rbind, rows)
summary_df <- summary_df[order(summary_df$scenario_id, summary_df$method), ]

write.csv(summary_df, file = here("results", "summary_table.csv"), row.names = FALSE)

## Ensure nice types for plotting
summary_df$n <- factor(summary_df$n, levels = sort(unique(summary_df$n)))
summary_df$error_type <- factor(summary_df$error_type, levels = c("normal", "t3"))
summary_df$method <- factor(summary_df$method, levels = c("wald", "boot_percentile", "boot_t"))

## ---- plots: facet by error_type (rows) and n (cols) ----

# Coverage
p2 <- ggplot(summary_df, aes(x = beta_treat, y = coverage, color = method, group = method)) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  geom_point() +
  geom_line() +
  facet_grid(error_type ~ n) +
  labs(
    x = expression(beta[treatment]),
    y = "Coverage (95% CI)",
    title = "Coverage by beta_treat, method, n, and error distribution"
  )

ggsave(filename = here("results", "coverage_by_scenario.png"), plot = p2, width = 11, height = 7)

# Computation time
p3 <- ggplot(summary_df, aes(x = beta_treat, y = mean_time, color = method, group = method)) +
  geom_point() +
  geom_line() +
  facet_grid(error_type ~ n) +
  labs(
    x = expression(beta[treatment]),
    y = "Mean time (sec)",
    title = "Computation time by beta_treat, method, n, and error distribution"
  )

ggsave(filename = here("results", "time_by_scenario.png"), plot = p3, width = 11, height = 7)

## ---- SE distribution (model-based SE from lm), facet by error_type ~ n ----
se_rows <- list()

for (f in files) {
  obj <- readRDS(f)
  sc <- obj$scenario
  
  se_rows[[length(se_rows) + 1]] <- data.frame(
    scenario_id = sc$scenario_id,
    n = sc$n,
    beta_treat = sc$beta_treat,
    error_type = sc$error_type,
    se_hat = obj$reps$se_hat,
    stringsAsFactors = FALSE
  )
}

se_df <- do.call(rbind, se_rows)
se_df$n <- factor(se_df$n, levels = sort(unique(se_df$n)))
se_df$error_type <- factor(se_df$error_type, levels = c("normal", "t3"))

p4 <- ggplot(se_df, aes(x = se_hat, fill = factor(beta_treat))) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_grid(error_type ~ n, scales = "free_y") +
  labs(
    x = "se(beta_hat) from lm",
    y = "Count",
    fill = expression(beta[treatment]),
    title = "Distribution of se(beta_hat) by n and error distribution (colors = beta_treat)"
  )

ggsave(filename = here("results", "se_distribution.png"), plot = p4, width = 11, height = 7)

message("DONE. Outputs saved in results/: summary_table.csv + 3 PNG plots")
