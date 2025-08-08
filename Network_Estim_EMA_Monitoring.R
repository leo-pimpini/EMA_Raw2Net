# ============================
# This script plots the input data, checks assumptions, and estimates networks (BS, temporal, contemp, random effects)
# Author: Leo Pimpini, June 16th 2025
# Adapted for EMA Monitoring study
# ============================

# Load Required Packages
library(dplyr)
library(readxl)
library(mlVAR)
library(qgraph)
library(tidyr)
library(ggplot2)
library(lubridate)
library(esmtools)
library(naniar)
library(tibble)

# 1. Load Data, set your path (use double backslashes or single forward single) 

output_dir <- "C:/Users/leonardo.pimpini/Desktop/PROJECTS/ONGOING/EMA_Monitoring/ANALYSES/Networks"
data_dir <- "C:/Users/leonardo.pimpini/Desktop/PROJECTS/ONGOING/EMA_Monitoring/ANALYSES/Networks/Data"
setwd(data_dir)

##################################
##################################
## IMPORTANT: Set input data here (ie, Low_freq, Medium_freq, High_freq)

df <- read_excel(file.path(data_dir, "High_freq.xlsx")) %>%
  rename(beep = Timepoint, day = Day_nr, id = ID)
##################################
##################################

df$id <- as.character(df$id)
vars <- c("CD", "CA", "SA", "Self_Control", "Stress")
df$Date_Time <- as.POSIXct(df$Date_Time)

# Convert all target variables to numeric
df <- df %>%
  mutate(across(all_of(vars), ~ as.numeric(.)))

#################################
# 2. Inspect variables of interest. For each, print: M, SD, range (min, max)
summary_df <- df %>%
  select(id, all_of(vars)) %>%
  pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    mean = mean(Value, na.rm = TRUE),
    sd = sd(Value, na.rm = TRUE),
    min = min(Value, na.rm = TRUE),
    max = max(Value, na.rm = TRUE),
    n = n_distinct(id[!is.na(Value)])
  ) %>%
  mutate(range = paste0(min, " – ", max)) %>%
  select(Variable, mean, sd, range, n)

print(summary_df, n = Inf)

############################

# 3. Plot variable distribution (per-participant average)
plot_dir <- file.path(output_dir, "Variables_distributions")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

avg_scores <- df %>%
  group_by(id) %>%
  summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = -id, names_to = "Variable", values_to = "Mean_Score")

avg_scores$Variable <- factor(avg_scores$Variable, 
                              levels = c("CF", "CA", "SA", "CD", "Self_Control", "Stress"))

for (v in levels(avg_scores$Variable)) {
  p <- ggplot(avg_scores %>% filter(Variable == v), aes(x = Variable, y = Mean_Score, label = id)) +
    geom_text(size = 3.5, position = position_jitter(width = 0.15, height = 0)) +
    theme_bw(base_size = 14) +
    labs(title = paste("Per-Participant Average:", v), y = "Average Score", x = NULL) +
    scale_y_continuous(limits = if (v %in% c("CD", "Self_Control", "Stress")) c(0, 100) else NULL)
  
  filename_safe <- paste0("Distribution_", gsub(" ", "_", v), ".png")
  ggsave(file.path(plot_dir, filename_safe), plot = p, width = 8, height = 6)
}

# 3b. Visualizes data per participant per assessment (simply to inspect the input data) 
df_long <- df %>%
  select(id, beep, all_of(vars)) %>%
  pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Score") %>%
  filter(!is.na(Score)) %>%
  group_by(id, Variable) %>%
  arrange(beep) %>%
  mutate(Timepoint = row_number()) %>%
  ungroup()

plot_group <- function(participant_id, vars_subset, title_suffix) {
  df_sub <- df_long %>% filter(id == participant_id, Variable %in% vars_subset)
  p <- ggplot(df_sub, aes(x = Timepoint, y = Score, color = Variable)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    theme_bw(base_size = 14) +
    labs(title = paste("Participant", participant_id, "-", title_suffix),
         x = "Assessment",
         y = "Score",
         color = "Variable") +
    theme(legend.position = "right")
  print(p)
}

for (participant in unique(df$id)) {
  plot_group(participant, c("CD", "Self_Control", "Stress"), "Craving Degree / Self Control / Stress")
  plot_group(participant, c("CA", "SA"), "Craving Amount / Snacking Amount")
}

# 4. Calendar Plots (just descriptive: when pp were assessed) 
heatcalendar_plot(df, "Date_Time")
calendar_plot(df, "Date_Time")

# 5. Missingness plot, per variable

vis_miss(df) +
  theme(axis.text.x = element_text(size = 9, angle = 80))

miss_var_summary(df)

gg_miss_case(df)

gg_miss_upset(df[, c('CD', 'CA', 'SA', 'Self_Control', 'Stress')], nsets = 5)

#########################################
#########################################
# IMPORTANT: FILTER OUT PP --> Enable this line below to (re)run the script without specific participants 
# 1 pp for Low freq = 94049
# 2 pp for Medium freq = 94063, 93935 
# 2 pp for High freq = 89631, 92266 

df <- df[!df$id %in% c(89631, 92266),]
#########################################
#########################################

# 6. Stationarity check (+ detrending if needed)

cat("\n===== STATIONARITY CHECK (Detrending) =====\n")

# Initialize tracking data frame for detrending
detrends <- data.frame(matrix(ncol = length(vars), nrow = length(unique(df$id))))
colnames(detrends) <- vars
rownames(detrends) <- as.character(unique(df$id))  # ensure row names are character

# Track variables excluded from detrending
excluded_detrend <- data.frame(id = character(), variable = character(), reason = character(), stringsAsFactors = FALSE)

# Loop through each participant
for (current_id in unique(df$id)) {
  row_indices <- which(df$id == current_id)
  pp <- df[row_indices, ]
  
  for (current_var in vars) {
    values <- pp[[current_var]]
    complete_cases <- complete.cases(values, pp$Date_Time)
    
    # Exclude pp if there is no variance
    if (length(unique(values[complete_cases])) <= 1) {
      excluded_detrend <- rbind(excluded_detrend, data.frame(id = current_id, variable = current_var, reason = "No variance"))
      next
    }
    
    # Exclude pp if too few data points were found, that is, less than 20 complete cases (ie, time-value pairs)
    if (sum(complete_cases) < 20) {
      excluded_detrend <- rbind(excluded_detrend, data.frame(id = current_id, variable = current_var, reason = "Too few observations (i.e., < 20)"))
      next
    }
    
    # Fit trend model
    fit <- lm(values[complete_cases] ~ pp$Date_Time[complete_cases])
    
    # If significant trend is observed, apply de-trending 
    if (anova(fit)$`Pr(>F)`[1] < 0.05) {
      detrends[as.character(current_id), current_var] <- 1
      residuals_adjusted <- residuals(fit) + mean(values[complete_cases], na.rm = TRUE)
      df[row_indices[complete_cases], current_var] <- residuals_adjusted
    } else {
      detrends[as.character(current_id), current_var] <- 0
    }
  }
}


# 7. Check for missing data (exclude pp with fewer than 20 observation in any variable) and flag (but doesn't discard) zero-variance variables 

# Count non-missing observations per (id, variable)
obs_summary <- df %>%
  pivot_longer(all_of(vars), names_to = "variable", values_to = "value") %>%
  group_by(id, variable) %>%
  summarise(n_obs = sum(!is.na(value)), .groups = "drop")

# Find IDs with any variable <20 obs
excluded_few_obs <- obs_summary %>%
  filter(n_obs < 20) %>%
  distinct(id) %>%
  pull(id)

# create a one-column table of excluded IDs (useful later, in section 9)
excluded_missing <- data.frame(id = excluded_few_obs)

# Report and drop those participants
cat("Excluding", length(excluded_few_obs),
    "participant(s) with <20 observations on at least one variable.\n")
df <- df %>% filter(!id %in% excluded_few_obs)

# Compute per-participant variance for each variable
zero_var_summary <- df %>%
  group_by(id) %>%
  summarise(across(all_of(vars), ~ var(.x, na.rm = TRUE),
                   .names = "var_{.col}"),
            .groups = "drop")

# Flag zero-variance pairs
excluded_zero_var <- zero_var_summary %>%
  pivot_longer(starts_with("var_"), names_to = "variable",
               values_to = "variance") %>%
  mutate(variable = sub("^var_", "", variable)) %>%
  filter(variance == 0)

cat("Identified", nrow(excluded_zero_var),
    "zero-variance variable(s) across participants.\n")


# Rename all variables for the network plots (var_labels)
vars <- c("CD", "CA", "SA", "Self_Control", "Stress")
var_labels <- c(
  "Craving Degree",
  "Craving Amount",
  "Snacking Amount",
  "Self-Control",
  "Stress"
)

# 8. Estimate 5 networks (contemp, temporal, BS, random effects contemp, random effects temporal)

mlVAR_res <- mlVAR(df, vars = vars, idvar = "id", dayvar = "day", beepvar = "beep", estimator = "lmer")

cont <- getNet(mlVAR_res, "contemporaneous", nonsig = "hide", rule = "and")
temp <- getNet(mlVAR_res, "temporal", nonsig = "hide")
bet  <- getNet(mlVAR_res, "between", nonsig = "hide", rule = "and")
Layout <- averageLayout(cont, temp, bet)

# 8b. Estimate random effects contemp. and temporal networks (individual SDs) 

SD_temp <- getNet(mlVAR_res, "temporal", SD = TRUE, lag = 1, partial = FALSE)
SD_contemp <- getNet(mlVAR_res, "contemporaneous", SD = TRUE, partial = FALSE)
# NOTE: BS random effects network is not available in mlVAR.

network_plot_dir <- file.path(output_dir, "Network_plots")
if (!dir.exists(network_plot_dir)) dir.create(network_plot_dir, recursive = TRUE)

png(file.path(network_plot_dir, "RandomEffects_Temporal.png"), width = 1400, height = 1200, res = 200)
qgraph(SD_temp, layout = Layout, theme = "colorblind", labels = var_labels, edge.color = "gray", fade = TRUE, edge.labels = TRUE, edge.label.cex = 1, edge.label.position = 0.5, vsize = 11, label.cex = 2.3, label.scale.equal = TRUE, mar = c(6,6,6,20), title = "SDs of Temporal random effects")
dev.off()

png(file.path(network_plot_dir, "RandomEffects_Contemporaneous.png"), width = 1400, height = 1200, res = 200)
qgraph(SD_contemp, layout = Layout, theme = "colorblind", labels = var_labels, edge.color = "gray", fade = TRUE, edge.labels = TRUE, edge.label.cex = 1, edge.label.position = 0.5, vsize = 11, label.cex = 2.3, label.scale.equal = TRUE, mar = c(6,6,6,20), title = "SDs of Contemporaneous random effects")
dev.off()


# 9. Plot Networks (5 in tot)
plot_network <- function(net, title, filename) {
  png(file.path(network_plot_dir, filename), width = 1400, height = 1200, res = 200)
  qgraph(net,
         layout = "circular",     # Use spring layout for better spacing betw nodes
         repulsion = 1.5,       # Increase repulsion for more equal spacing betw nodes
         theme = "colorblind",
         labels = var_labels,
         label.scale.equal = TRUE,
         vsize = 11,
         label.cex = 2.3,
         asize = 4.5,
         border.width = 3,
         border.color = "gray35",
         edge.labels = TRUE,
         edge.label.cex = 1,
         edge.label.position = 0.5,
         fade = FALSE,
         mar = c(6, 6, 6, 20),
         color = ifelse(vars %in% c("CD", "CA", "SA"), "lightyellow", "lightblue"),
         title = title)
  dev.off()
}

plot_network(cont, "Contemporaneous Network", "Contemporaneous_Network.png")
plot_network(temp, "Temporal Network", "Temporal_Network.png")
plot_network(bet,  "BS Network", "BS_Network.png")

# 10. Save Exclusion Reports
#excluded_detrend$id <- as.character(excluded_detrend$id)
excluded_missing$id <- as.character(excluded_missing$id)
excluded_zero_var$id <- as.character(excluded_zero_var$id)

all_exclusions <- bind_rows(
#  excluded_detrend,
  excluded_missing,
  excluded_zero_var
)

excl_dir <- file.path(output_dir, "Exclusion_reports")
if (!dir.exists(excl_dir)) dir.create(excl_dir, recursive = TRUE)

#write.table(excluded_detrend, file = file.path(excl_dir, "excluded_due_to_detrending.csv"), sep = ",", row.names = FALSE, col.names = TRUE)
write.table(excluded_missing, file = file.path(excl_dir, "excluded_due_to_missingness.csv"), sep = ",", row.names = FALSE, col.names = TRUE)
write.table(excluded_zero_var, file = file.path(excl_dir, "excluded_due_to_zero_variance.csv"), sep = ",", row.names = FALSE, col.names = TRUE)
# write.table(all_exclusions, file = file.path(excl_dir, "excluded_all_combined.csv"), sep = ",", row.names = FALSE, col.names = TRUE)

cat("\n===== SCRIPT COMPLETE. Network plots + variable distributions + reports are saved in:\n", output_dir, " =====\n")
