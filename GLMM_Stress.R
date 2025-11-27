# ================================
# LMM for Stress (continuous)
# EMA Monitoring study
# ================================

# ---- Packages ----
library(readxl)
library(dplyr)
library(lubridate)
library(lme4)
library(ggplot2)
library(writexl)
library(emmeans)
# Optional diagnostics:
# if (!requireNamespace("DHARMa", quietly = TRUE)) install.packages("DHARMa")
# library(DHARMa)

# ---- Inputs ----
id_col          <- "ID"
time_col        <- "Date_Time"
base_path       <- "C:/Users/leonardo.pimpini/Desktop/PROJECTS/ONGOING/EMA_Monitoring/ANALYSES/GLM/Data"
merged_file     <- "EMA_merged_AllFreq.xlsx"

# ---- Read & Prepare Data ----
dat <- read_xlsx(file.path(base_path, merged_file))

stopifnot(all(c(id_col, time_col, "Stress") %in% names(dat)))

dat <- dat %>%
  rename(ID = all_of(id_col),
         Date_Time = all_of(time_col)) %>%
  mutate(
    Date_Time = parse_date_time(
      Date_Time,
      orders = c("Ymd HMS","Ymd HM","Ymd","dmY HMS","dmY HM")
    ),
    Group  = factor(Group, levels = c("Low","Med","High")),
    Stress = as.numeric(Stress)
  ) %>%
  arrange(ID, Date_Time)

# Time since first obs within ID (days)
dat <- dat %>%
  group_by(ID) %>%
  mutate(Time = as.numeric(difftime(Date_Time, min(Date_Time, na.rm = TRUE), units = "days"))) %>%
  ungroup()

# ---- Descriptives ----
vars_to_sum <- c("Stress","Time")

summary_overall <- dat %>%
  summarise(across(all_of(vars_to_sum),
                   list(N = ~sum(!is.na(.)),
                        M = ~mean(., na.rm = TRUE),
                        SD = ~sd(., na.rm = TRUE),
                        Min = ~min(., na.rm = TRUE),
                        Max = ~max(., na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))
print(summary_overall)

summary_by_group <- dat %>%
  group_by(Group) %>%
  summarise(across(all_of(vars_to_sum),
                   list(N = ~sum(!is.na(.)),
                        M = ~mean(., na.rm = TRUE),
                        SD = ~sd(., na.rm = TRUE),
                        Min = ~min(., na.rm = TRUE),
                        Max = ~max(., na.rm = TRUE)),
                   .names = "{.col}_{.fn}"),
            .groups = "drop")
print(summary_by_group)

# Quick mean/variance info
m_ST <- mean(dat$Stress, na.rm = TRUE)
v_ST <- var(dat$Stress,  na.rm = TRUE)
cat("Mean(Stress) =", m_ST, " | Var(Stress) =", v_ST, "\n")

# Distribution plot
ggplot(dat, aes(x = Stress)) +
  geom_histogram(binwidth = diff(range(dat$Stress, na.rm = TRUE))/30, color = "white") +
  labs(title = "Distribution of Stress",
       x = "Stress", y = "Frequency") +
  theme_minimal()

# ---- Model (Gaussian LMM) ----
form_lmm <- as.formula("Stress ~ Time * Group + (Time | ID)")

m <- lmer(form_lmm, data = dat, REML = FALSE)

# AIC (for record-keeping)
cat("Model AIC:", AIC(m), "\n")

# Print fitted model summary
summary(m)

# ---- Raw-data plot for 20 sampled IDs per group (robust sampling) ----
set.seed(123)

# Shuffle within group, then take up to 20 IDs per group
sampled_ids <- dat %>%
  distinct(Group, ID) %>%
  group_by(Group) %>%
  mutate(.rand = runif(n())) %>%   # random shuffle
  arrange(.rand, .by_group = TRUE) %>%
  slice(1:min(20, dplyr::n())) %>%
  ungroup() %>%
  select(-.rand)

dat_sub <- dat %>%
  inner_join(sampled_ids, by = c("Group","ID"))

ggplot(dat_sub, aes(x = Time, y = Stress, color = Group)) +
  geom_point(aes(group = ID), alpha = 0.25, size = 0.6) +
  geom_smooth(
    aes(fill = Group),
    method = "lm",    # linear smoother for continuous outcome
    se = TRUE, linewidth = 1, alpha = 0.2
  ) +
  labs(
    title = "Stress (n = 20 per group)",
    x = "Time (days)",
    y = "Stress (VAS)"
  ) +
  theme_minimal()

# ---- Predicted trajectories (group means over time) with 95% CIs on raw scale ----
time_grid <- seq(min(dat$Time, na.rm = TRUE),
                 max(dat$Time, na.rm = TRUE),
                 length.out = 100)

# emmeans on response scale (identity link for Gaussian LMM)
pred_emm <- emmeans(m, ~ Group | Time, at = list(Time = time_grid), type = "response")
pred_raw <- as.data.frame(pred_emm)

# Robust column mapping (handles emmeans version differences)
mean_col <- if ("emmean" %in% names(pred_raw)) "emmean" else if ("response" %in% names(pred_raw)) "response" else stop("No mean column found in emmeans output.")
lcl_col  <- dplyr::case_when(
  "lower.CL"  %in% names(pred_raw) ~ "lower.CL",
  "asymp.LCL" %in% names(pred_raw) ~ "asymp.LCL",
  "LCL"       %in% names(pred_raw) ~ "LCL",
  TRUE ~ NA_character_
)
ucl_col  <- dplyr::case_when(
  "upper.CL"  %in% names(pred_raw) ~ "upper.CL",
  "asymp.UCL" %in% names(pred_raw) ~ "asymp.UCL",
  "UCL"       %in% names(pred_raw) ~ "UCL",
  TRUE ~ NA_character_
)

pred_df <- pred_raw %>%
  mutate(
    emmean = .data[[mean_col]],
    lower  = if (!is.na(lcl_col)) .data[[lcl_col]] else NA_real_,
    upper  = if (!is.na(ucl_col)) .data[[ucl_col]] else NA_real_
  )

stopifnot(all(c("Time","Group","emmean") %in% names(pred_df)))

ggplot(pred_df, aes(x = Time, y = emmean, color = Group, fill = Group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.20, linewidth = NA, na.rm = TRUE) +
  geom_line(linewidth = 1) +
  labs(
    title = "Stress (all participants)",
    x = "Time (days)",
    y = "Stress (VAS)"
  ) +
  theme_minimal()
