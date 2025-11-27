# Generalized Linear Model for EMA Monitoring study

# GLM over Craving Degree (CD; gaussian) 

# Packages
library(readxl)
library(dplyr)
library(lubridate)
library(lme4)
library(writexl)
library(ggplot2)  
library(tidyr)

# Input data
id_col   <- "ID"
time_col <- "Date_Time"
base_path <- "C:/Users/leonardo.pimpini/Desktop/PROJECTS/ONGOING/EMA_Monitoring/ANALYSES/GLM/Data"

# Read files
low  <- read_xlsx(file.path(base_path, "Low_freq.xlsx"))  %>% mutate(Group = "Low")
med  <- read_xlsx(file.path(base_path, "Medium_freq.xlsx")) %>% mutate(Group = "Med")
high <- read_xlsx(file.path(base_path, "High_freq.xlsx")) %>% mutate(Group = "High")

# Merge datasets
dat <- bind_rows(low, med, high)

# Standardize columns & types 
stopifnot(all(c(id_col, time_col, "CD") %in% names(dat)))

dat <- dat %>%
  rename(ID = all_of(id_col), Date_Time = all_of(time_col)) %>%
  mutate(
    # Parse timestamp (handles common formats; adjust orders if needed)
    Date_Time = parse_date_time(Date_Time, orders = c("Ymd HMS", "Ymd HM", "Ymd", "dmY HMS", "dmY HM")),
    Group = factor(Group, levels = c("Low", "Med", "High")),
    CD = as.numeric(CD)
  ) %>%
  arrange(ID, Date_Time)

# Create Time_ti (elapsed days since first observation within ID) 
dat <- dat %>%
  group_by(ID) %>%
  mutate(Time = as.numeric(difftime(Date_Time, min(Date_Time, na.rm = TRUE), units = "days"))) %>%
  ungroup()

# Variable summaries (overall and by Group): N, M, SD, range (min, max)
vars_to_sum <- c("CD", "Time")
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

# save merged dataset for next scripts
write_xlsx(dat, path = file.path(base_path, "EMA_merged_AllFreq.xlsx"))

# Fit generalized linear (Gaussian) mixed model 
# Model: Y_ti = β0 + β1*Time_ti + β2*Group_Med + β3*Group_High
#                  + β4*(Time_ti×Group_Med) + β5*(Time_ti×Group_High) + u0i + e_ti
# Implemented via: CD ~ Time * Group + (1 | ID), with "Low" as reference

m <- lmer(CD ~ Time * Group + (1 | ID), data = dat, REML = FALSE)

# Results 
summary(m)

# coefficient table only:
# coef(summary(m))

# extract and print the fixed effects neatly:
fixef(m)


# ----- Group-specific Time slopes with df-adjusted p-values -----
if (!requireNamespace("emmeans", quietly = TRUE)) install.packages("emmeans")
library(emmeans)

# Allow df-adjusted tests for large datasets
emm_options(pbkrtest.limit = 50000, lmerTest.limit = 50000)

# Slope of Time within each Group (Low = b1; Med = b1+b4; High = b1+b5)
trends <- emtrends(m, specs = "Group", var = "Time")
print(summary(trends))          # df, t, p
print(confint(trends, level=0.95))
print(pairs(trends))            # slope differences across groups


# Plot GLM results using raw data (+ trend & ribbon) from 20 randomly selected IDs per condition

set.seed(123)  # reproducible sampling

# sample up to 20 IDs per group
sampled_ids <- dat %>%
  group_by(Group) %>%
  summarise(IDs = list(unique(ID)), .groups = "drop") %>%
  rowwise() %>%
  mutate(IDs = list(sample(IDs, size = min(20, length(IDs))))) %>%
  unnest(IDs) %>%
  rename(ID = IDs)

dat_sub <- dat %>%
  inner_join(sampled_ids, by = c("Group", "ID"))

ggplot(dat_sub, aes(x = Time, y = CD, color = Group)) +
  geom_point(aes(group = ID), alpha = 0.25, size = 0.6) +
  geom_smooth(aes(fill = Group), method = "lm", se = TRUE, linewidth = 1, alpha = 0.2) +
  labs(title = "Craving degree (n = 20 per group)",
       x = "Time (days)",
       y = "Craving degree (VAS)") +
  theme_minimal()


# ---- NEW: Predicted trajectories (group means over time) with 95% CIs on raw scale ----
# Uses model-based marginal means from emmeans across a sequence of observed Time values.

# Create an evenly spaced grid over the observed Time range
time_grid <- seq(min(dat$Time, na.rm = TRUE),
                 max(dat$Time, na.rm = TRUE),
                 length.out = 100)

# Obtain predicted marginal means per Group at each Time point (Gaussian identity => raw scale)
pred_emm <- emmeans(m, ~ Group | Time, at = list(Time = time_grid), type = "response")
pred_df  <- as.data.frame(pred_emm) %>%
  rename(emmean = emmean, lower = lower.CL, upper = upper.CL)

# Plot predicted trajectories with 95% CIs
ggplot(pred_df, aes(x = Time, y = emmean, color = Group, fill = Group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.20, linewidth = NA) +
  geom_line(linewidth = 1) +
  labs(title = "Craving degree (all participants)",
       x = "Time (days)",
       y = "Craving degree (VAS)") +
  theme_minimal()
