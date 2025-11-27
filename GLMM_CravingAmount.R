# ================================
# GLMM for Craving Amount (CA; count)
# EMA Monitoring study
# ================================

# ---- Packages ----
library(readxl)
library(dplyr)
library(lubridate)
library(lme4)
library(glmmTMB)
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

stopifnot(all(c(id_col, time_col, "CA") %in% names(dat)))

dat <- dat %>%
  rename(ID = all_of(id_col),
         Date_Time = all_of(time_col)) %>%
  mutate(
    Date_Time = parse_date_time(
      Date_Time,
      orders = c("Ymd HMS","Ymd HM","Ymd","dmY HMS","dmY HM")
    ),
    Group = factor(Group, levels = c("Low","Med","High")),
    CA    = as.numeric(CA)
  ) %>%
  arrange(ID, Date_Time)

# Time since first obs within ID (days)
dat <- dat %>%
  group_by(ID) %>%
  mutate(Time = as.numeric(difftime(Date_Time, min(Date_Time, na.rm = TRUE), units = "days"))) %>%
  ungroup()


# ---- Descriptives ----
vars_to_sum <- c("CA","Time")

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

# Quick variance vs mean check
m_CA <- mean(dat$CA, na.rm = TRUE)
v_CA <- var(dat$CA,  na.rm = TRUE)
cat("Mean(CA) =", m_CA, " | Var(CA) =", v_CA, "\n")

# Distribution plot
ggplot(dat, aes(x = CA)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(title = "Distribution of Craving Amount (CA)",
       x = "Count of cravings", y = "Frequency") +
  theme_minimal()


# ---- Zero counts ----
zero_summary <- dat %>%
  mutate(is_zero = CA == 0) %>%
  group_by(Group) %>%
  summarise(
    n_zeros = sum(is_zero, na.rm = TRUE),
    total = sum(!is.na(CA)),
    pct_zeros = 100 * n_zeros / total
  ) %>%
  bind_rows(
    dat %>%
      summarise(
        Group = "Overall",
        n_zeros = sum(CA == 0, na.rm = TRUE),
        total = sum(!is.na(CA)),
        pct_zeros = 100 * n_zeros / total
      )
  )

print(zero_summary)


# ---- Model Candidates & Selection ----
form_glmm <- as.formula("CA ~ Time * Group + (Time | ID)")

fit_safe <- function(name, fam, zi = FALSE) {
  zif <- if (zi) ~ 1 else ~ 0
  mod <- tryCatch(
    glmmTMB(form_glmm, data = dat, family = fam, ziformula = zif),
    error = function(e) NULL
  )
  if (is.null(mod)) return(list(info = NULL, model = NULL))
  info <- data.frame(
    Model     = name,
    Converged = isTRUE(mod$fit$convergence == 0),
    logLik    = as.numeric(logLik(mod)),
    df        = attr(logLik(mod), "df"),
    AIC       = AIC(mod),
    stringsAsFactors = FALSE
  )
  list(info = info, model = mod)
}

cand_list <- list(
  fit_safe("Poisson",                  poisson(), zi = FALSE),
  fit_safe("Zero-Infl Poisson (ZIP)", poisson(), zi = TRUE),
  fit_safe("NegBin (nbinom2)",        nbinom2(),  zi = FALSE),
  fit_safe("Zero-Infl NegBin (ZINB)", nbinom2(),  zi = TRUE)
)

infos <- lapply(cand_list, `[[`, "info") |> Filter(Negate(is.null), x = _)
if (length(infos) == 0) stop("No candidate models could be estimated.")

aic_table <- dplyr::bind_rows(infos) %>%
  arrange(AIC) %>%
  mutate(DeltaAIC = AIC - min(AIC, na.rm = TRUE))
print(aic_table)

best_name <- aic_table$Model[1]
cat("Best (lowest AIC):", best_name,
    "| AIC =", aic_table$AIC[1],
    "| logLik =", aic_table$logLik[1], "\n")

models <- lapply(cand_list, `[[`, "model")
names(models) <- sapply(cand_list, function(x) if (is.null(x$info)) NA_character_ else x$info$Model)
models <- models[!is.na(names(models))]
best_model <- models[[best_name]]
if (is.null(best_model)) stop("Best model object not found.")

# Optional dispersion check if Poisson is chosen
if (best_name == "Poisson") {
  pr  <- resid(best_model, type = "pearson")
  k   <- length(fixef(best_model)$cond)
  n_e <- sum(is.finite(pr))
  rdf <- max(n_e - k, 1)
  disp <- sum(pr^2, na.rm = TRUE) / rdf
  cat(sprintf("Poisson dispersion statistic (Pearson chi^2/df): %.3f\n", disp))
  if (disp > 1.5) cat("Overdispersion (>~1.5) detected; NB/ZINB usually preferable if comparable.\n")
  # DHARMa alternative:
  # sim <- DHARMa::simulateResiduals(best_model, plot = FALSE); DHARMa::testDispersion(sim)
}

# Print fitted model summary
summary(best_model)

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

ggplot(dat_sub, aes(x = Time, y = CA, color = Group)) +
  geom_point(aes(group = ID), alpha = 0.25, size = 0.6) +
  geom_smooth(
    aes(fill = Group),
    method = "glm",
    method_args = list(family = poisson(link = "log")),  # exploratory smooth
    se = TRUE, linewidth = 1, alpha = 0.2
  ) +
  labs(
    title = "Craving amount (n = 20 per group)",
    x = "Time (days)",
    y = "Craving amount (portions)"
  ) +
  theme_minimal()

# ---- Predicted trajectories (group means over time) with 95% CIs on raw scale ----
time_grid <- seq(min(dat$Time, na.rm = TRUE),
                 max(dat$Time, na.rm = TRUE),
                 length.out = 100)

# emmeans on response scale (works for Poisson/NB/ZIP/ZINB in glmmTMB)
pred_emm <- emmeans(best_model, ~ Group | Time, at = list(Time = time_grid), type = "response")
pred_raw <- as.data.frame(pred_emm)

# Robust column mapping (different emmeans/glmmTMB versions name CI columns differently)
mean_col <- if ("response" %in% names(pred_raw)) "response" else if ("emmean" %in% names(pred_raw)) "emmean" else stop("No mean column found in emmeans output.")
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
    title = "Craving amount (all participants)",
    x = "Time (days)",
    y = "Craving amount (portions)"
  ) +
  theme_minimal()


