# Wage Determination Among Informal Domestic Workers in Delhi
# Tripta Behera, 2026
#
# Primary survey of 19 female Muslim migrant domestic workers,
# Jai Hind Camp, Vasant Kunj, New Delhi (March 2025)
#
# The analysis examines wage levels of 16 workers relative to Delhi's statutory
# minimum wage, the degree of wage dispersion, and whether
# human capital variables (education, experience and task type)
# explain observed wage variation among workers in this settlement.


library(stargazer)
library(dplyr)
library(ggplot2)


# DATA

df <- read.csv("Migration_DataEntry_stata.csv")

# Hourly wage is constructed as monthly earnings divided by the product
# of each worker's reported working hours and actual labour days that month.
# Workers reported between 28-30 labour days; individual figures are used
# rather than assuming a uniform 28-day month.
df$hourly_wage     <- df$earnings / (df$length_working_day * df$labour_days)
df$log_hourly_wage <- log(df$hourly_wage)

# The minimum wage benchmark is the Delhi Labour Department notification
# for unskilled workers dated 01/10/2024 (Rs 695/day). This is converted
# to an hourly figure assuming an 8-hour standard working day.
min_wage_hourly <- 695 / 8


# SUMMARY STATISTICS

summary_vars <- df[, c("hourly_wage", "earnings", "yrs_school",
                        "yrs_migration_num", "length_working_day")]

stargazer(summary_vars,
          type = "text",
          title = "Table 1: Summary Statistics",
          covariate.labels = c("Hourly Wage (Rs)",
                               "Monthly Earnings (Rs)",
                               "Years of Schooling",
                               "Years Since Migration",
                               "Hours Worked per Day"),
          digits = 2,
          digit.separate = 0,
          notes = "Education = 0 denotes no formal schooling. Sample excludes two non-working respondents and one waste segregation worker.",
          notes.align = "l")

stargazer(summary_vars,
          type = "html",
          out = "summary_table.html",
          title = "Table 1: Summary Statistics",
          covariate.labels = c("Hourly Wage (Rs)",
                               "Monthly Earnings (Rs)",
                               "Years of Schooling",
                               "Years Since Migration",
                               "Hours Worked per Day"),
          digits = 2,
          digit.separate = 0,
          notes = "Education = 0 denotes no formal schooling. Sample excludes two non-working respondents and one waste segregation worker.",
          notes.align = "l")


# MINIMUM WAGE COMPARISON

cat("\nMinimum wage comparison\n")
cat("Delhi minimum wage (unskilled):  Rs", round(min_wage_hourly, 2), "per hour\n")
cat("Sample mean hourly wage:         Rs", round(mean(df$hourly_wage), 2), "\n")
cat("Sample median hourly wage:       Rs", round(median(df$hourly_wage), 2), "\n")
cat("Workers below minimum wage:      ",
    sum(df$hourly_wage < min_wage_hourly), "of", nrow(df), "\n")
cat("Mean deficit relative to minimum:",
    round((min_wage_hourly - mean(df$hourly_wage)) / min_wage_hourly * 100, 1), "%\n")


# WAGE DISPERSION

cat("\nWage dispersion\n")
cat("Standard deviation:              Rs", round(sd(df$hourly_wage), 2), "\n")
cat("Coefficient of variation:        ", round(sd(df$hourly_wage) / mean(df$hourly_wage), 3), "\n")
cat("10th percentile:                 Rs", round(quantile(df$hourly_wage, 0.1), 2), "\n")
cat("90th percentile:                 Rs", round(quantile(df$hourly_wage, 0.9), 2), "\n")
cat("90/10 ratio:                     ",
    round(quantile(df$hourly_wage, 0.9) / quantile(df$hourly_wage, 0.1), 2), "\n")


# REGRESSION
# Three OLS models with log hourly wage as the dependent variable.
# Model 1 tests whether task type alone predicts wages.
# Model 2 adds years of schooling and years since migration.
# Model 3 adds a dummy for reported differential treatment by employer
# on the basis of religion, to test whether discrimination operates
# through wage suppression within employment.
# The reference category for task dummies is cleaning only.

model1 <- lm(log_hourly_wage ~ task_cooking + task_utensils,
             data = df, na.action = na.omit)

model2 <- lm(log_hourly_wage ~ task_cooking + task_utensils +
               yrs_school + yrs_migration_num,
             data = df, na.action = na.omit)

model3 <- lm(log_hourly_wage ~ task_cooking + task_utensils +
               yrs_school + yrs_migration_num +
               differential_treatment_employer,
             data = df, na.action = na.omit)

stargazer(model1, model2, model3,
          type = "text",
          title = "Table 2: OLS Regression - Determinants of Log Hourly Wage",
          dep.var.labels = "Log Hourly Wage",
          order = c("task_cooking", "task_utensils",
                    "yrs_school", "yrs_migration_num",
                    "differential_treatment_employer"),
          covariate.labels = c("Task: Cooking (=1)",
                               "Task: Utensils (=1)",
                               "Years of Schooling",
                               "Years Since Migration",
                               "Differential Treatment (=1)"),
          omit.stat = c("f", "ser", "adj.rsq"),
          digits = 3,
          notes = "Reference category: cleaning only. Standard errors in parentheses.",
          notes.align = "l")

stargazer(model1, model2, model3,
          type = "html",
          out = "regression_table.html",
          title = "Table 2: OLS Regression — Determinants of Log Hourly Wage",
          dep.var.labels = "Log Hourly Wage",
          order = c("task_cooking", "task_utensils",
                    "yrs_school", "yrs_migration_num",
                    "differential_treatment_employer"),
          covariate.labels = c("Task: Cooking (=1)",
                               "Task: Utensils (=1)",
                               "Years of Schooling",
                               "Years Since Migration",
                               "Differential Treatment (=1)"),
          omit.stat = c("f", "ser", "adj.rsq"),
          digits = 3,
          notes = "Reference category: cleaning only. Standard errors in parentheses.",
          notes.align = "l")


# FIGURES

theme_wage <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, margin = margin(b = 6)),
    plot.subtitle    = element_text(size = 11, color = "grey40", margin = margin(b = 12)),
    plot.caption     = element_text(size = 9, color = "grey50", hjust = 0, margin = margin(t = 10)),
    axis.title       = element_text(size = 11, color = "grey30"),
    axis.text        = element_text(size = 10, color = "grey40"),
    panel.grid.major = element_line(color = "grey92"),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(16, 16, 16, 16)
  )


# Figure 1: Hourly wages ranked in ascending order with minimum wage benchmark
# Workers are ranked lowest to highest to visualise the full wage distribution
# and how many fall below the statutory minimum.

fig1_data <- data.frame(
  rank = seq_along(sort(df$hourly_wage)),
  wage = sort(df$hourly_wage),
  below_min = sort(df$hourly_wage) < min_wage_hourly
)

fig1 <- ggplot(fig1_data, aes(x = rank, y = wage, fill = below_min)) +
  geom_col(width = 0.75, colour = "white", linewidth = 0.3) +
  geom_hline(yintercept = min_wage_hourly,
             colour = "#C0392B", linewidth = 0.8, linetype = "dashed") +
  annotate("text",
           x     = max(fig1_data$rank) - 1.5,
           y     = min_wage_hourly + 4,
           label = paste0("Min wage: Rs ", round(min_wage_hourly, 0), "/hr"),
           colour = "#C0392B", size = 3.5, hjust = 1) +
  scale_fill_manual(
    values = c("TRUE" = "#2C5F8A", "FALSE" = "#85B8D9"),
    labels = c("TRUE" = "Below minimum wage", "FALSE" = "At or above minimum wage"),
    name   = NULL
  ) +
  scale_x_continuous(breaks = fig1_data$rank, labels = fig1_data$rank) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     limits = c(0, 120)) +
  labs(
    title    = "Figure 1: Hourly Wages Ranked in Ascending Order",
    subtitle = "16 female domestic workers, Jai Hind Camp, Vasant Kunj (March 2025)",
    x        = "Worker (ranked by hourly wage)",
    y        = "Hourly Wage (Rs)",
    caption  = "Dashed line: Delhi minimum wage for unskilled workers (Rs 695/day, notified 01/10/2024),\nconverted to hourly assuming 8-hour standard day. Darker bars = below minimum wage."
  ) +
  theme_wage +
  theme(legend.position  = "top",
        legend.text      = element_text(size = 10),
        panel.grid.major.x = element_blank())

ggsave("wage_dispersion_chart.png", fig1, width = 9, height = 5.5,
       dpi = 300, bg = "white")


# Figure 2: Education vs hourly wage
# A flat or negative trend line is evidence against the human capital
# prediction that wages rise with education in this labour market.

fig2 <- ggplot(df, aes(x = yrs_school, y = hourly_wage)) +
  geom_hline(yintercept = min_wage_hourly,
             colour = "grey65", linewidth = 0.8, linetype = "dotted") +
  annotate("text",
           x     = 10,
           y     = min_wage_hourly + 4,
           label = paste0("Min wage: Rs ", round(min_wage_hourly, 0), "/hr"),
           colour = "grey50", size = 3.3, hjust = 1) +
  geom_smooth(method = "lm", se = TRUE,
              colour  = "#C0392B",
              fill    = "#C0392B",
              alpha   = 0.12,
              linewidth = 0.9,
              linetype  = "dashed") +
  geom_point(colour = "#2C5F8A", size = 3.5, alpha = 0.85) +
  scale_x_continuous(limits = c(-0.5, 11),
                     breaks  = seq(0, 10, 2)) +
  scale_y_continuous(limits  = c(0, 120),
                     expand  = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Figure 2: Education and Hourly Wage",
    subtitle = "16 female domestic workers, Jai Hind Camp, Vasant Kunj (March 2025)",
    x        = "Years of Schooling",
    y        = "Hourly Wage (Rs)",
    caption  = "Dashed line: OLS trend with 95% confidence interval.\nDotted line: Delhi minimum wage (unskilled, Rs 87/hr). Education = 0 denotes no formal schooling."
  ) +
  theme_wage

ggsave("education_wage_scatter.png", fig2, width = 8, height = 5.5,
       dpi = 300, bg = "white")

cat("\nFigures saved: wage_dispersion_chart.png, education_wage_scatter.png\n")
