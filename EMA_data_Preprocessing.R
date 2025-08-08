
# Pre-processing script starting from EMA raw data (all pp merged; one file per BS condition) 
# Based on:  EMA Monitoring study 
# Author: Leo Pimpini, June 13th 2025

library(readxl)
library(dplyr)
library(openxlsx)
library(lubridate)


################################################
# 1. Set input file name  (change to "Low_F", "Medium_F," "High_F" as needed)
target_group <- "High_F"  
###############################################


# 1b. Set input and output file path   
input_file  <- paste0("C:/Users/leonardo.pimpini/Desktop/Cleaned_Data_EMA_Monitoring/Readin_MergeAllBatches/Merged_raw_data_", target_group, ".xlsx")
file_suffix <- paste0("Cleaned_raw_data_", target_group, ".xlsx")
output_dir  <- "C:/Users/leonardo.pimpini/Desktop/Cleaned_Data_EMA_Monitoring/Step2_Cleaned_Data"
output_file <- file.path(output_dir, file_suffix)

# 2. Read in Excel and drop unnamed columns 
raw <- read_excel(input_file, col_types = "text")
valid_names <- names(raw)
valid_names[is.na(valid_names) | valid_names == ""] <- NA      # mark blanks
raw <- raw[, !is.na(valid_names)]                               # keep named cols
names(raw) <- make.names(valid_names[!is.na(valid_names)],      # unique, syntactic
                         unique = TRUE)
data <- raw   # work on a copy

# 3. Rename variables/columns 
names(data) <- gsub("Participant.ID", "ID", names(data))
names(data) <- gsub("Session.Scheduled.Time",  "Date_Time", names(data))
names(data) <- gsub("X.1_SAQ..1..Craving", "Craved_Y/N", names(data))

# Rename variables (Categories of craved snacks) 
names(data) <- gsub("X.2_MAQ_1..Sweet..chocolate.ice.cream.candies.",  "Craved_Cat_Sweet_Snacks", names(data))
names(data) <- gsub("X.2_MAQ_2..Savory..chips.crisps.pop.corn.pretzels.", "Craved_Cat_Savory_Snacks", names(data))
names(data) <- gsub("X.2_MAQ_3..Fried..french.fries.croquettes.bitterballen.", "Craved_Cat_Fried_Snacks", names(data))
names(data) <- gsub("X.2_MAQ_4..Dairy..cheese.pudding.milkshakes", "Craved_Cat_Dairy_Snacks",   names(data))
names(data) <- gsub("X.2_MAQ_5..Baked..cookies.muffin.croissants.cupcakes.","Craved_Cat_Baked_Snacks",  names(data))
names(data) <- gsub("X.2_MAQ_6..Dried.Fruits...Nuts..almond.walnuts.peanuts.","Craved_Cat_DriedFruitNuts_Snacks",  names(data))
names(data) <- gsub("X.2_MAQ_7..Bread.Crackers.Sandwich..nutella.jam.toast.", "Craved_Cat_Sandwick_Snacks", names(data))
names(data) <- gsub("X.2_MAQ_8..Energy.Granola.Bars", "Craved_Cat_EnergyBars_Snacks",names(data))
names(data) <- gsub("X.2_MAQ_9..Processed.Meat..sausage.and.meat.sticks.beef.jerky", "Craved_Cat_ProcessedMeat_Snacks", names(data))
names(data) <- gsub("X.2_MAQ_11..Other..please.specify.hereafter.",  "Craved_Cat_Others_Snacks", names(data))

# Rename variables (Craving amount per category) 
names(data) <- gsub("X.3_SAQ..1b..Craving.Amount.Sweet.Snacks",  "CA_Sweet_Snacks",  names(data))
names(data) <- gsub("X.4_SAQ..1c..Craving.Amount.Savory.Snacks", "CA_Savory_Snacks", names(data))
names(data) <- gsub("X.5_SAQ..1d..Craving.Amount.Fried.Snacks",  "CA_Fried_Snacks", names(data))
names(data) <- gsub("X.6_SAQ..1e..Craving.Amount.Dairy.Snacks", "CA_Dairy_Snacks",  names(data))
names(data) <- gsub("X.7_SAQ..1f..Craving.Amount.Baked.Snacks", "CA_Baked_Snacks", names(data))
names(data) <- gsub("X.8_SAQ..1g..Craving.Amount.Dried.Fruits...Nuts", "CA_DriedFruitNuts_Snacks",names(data))
names(data) <- gsub("X.9_SAQ..1h..Craving.Amount.Bread.Crackers.Sandwich", "CA_Sandwich_Snacks",  names(data))
names(data) <- gsub("X.10_SAQ..1i..Craving.Amount.Energy.Granola.Bars", "CA_EnergyBars_Snacks",  names(data))
names(data) <- gsub("X.11_SAQ..1j..Craving.Amount.Processed.Meat",  "CA_ProcessedMeat_Snacks", names(data))
names(data) <- gsub("X.14_SAQ..1m..Craving.Amount.Other.Snacks", "CA_Other_Snacks", names(data))

# Rename variables (Snacked on smth? Yes, no) 
names(data) <- gsub("X.16_SAQ..2..Intake", "Snacked_Y/N", names(data))

# Rename variables (Categories of eaten snacks)
#names(data) <- gsub("X.16_SAQ..2..Intake",  "Snacked_Cat_Sweet_Snacks",   names(data))
names(data) <- gsub("X.17_MAQ_1..Sweet..chocolate.ice.cream.candies", "Snacked_Cat_Sweet_Snacks", names(data))
names(data) <- gsub("X.17_MAQ_2..Savory..chips.crisps.pop.corn.pretzels.","Snacked_Cat_Savory_Snacks",  names(data))
names(data) <- gsub("X.17_MAQ_3..Fried..french.fries.croquettes.bitterballen.",
                    "Snacked_Cat_Fried_Snacks", names(data))
names(data) <- gsub("X.17_MAQ_4..Dairy..cheese.pudding.milkshakes.",   "Snacked_Cat_Dairy_Snacks",  names(data))
names(data) <- gsub("X.17_MAQ_5..Baked..cookies.muffin.croissants.cupcakes.",
                    "Snacked_Cat_Baked_Snacks",  names(data))
names(data) <- gsub("X.17_MAQ_6..Dried.Fruits...Nuts",  "Snacked_Cat_DriedFruitNuts_Snacks", names(data))
names(data) <- gsub("X.17_MAQ_7..Bread.Crackers.Sandwich..nutella.jam.toast.",
                    "Snacked_Cat_Sandwich_Snacks",  names(data))
names(data) <- gsub("X.17_MAQ_8..Energy.Granola.Bars",  "Snacked_Cat_EnergyBars_Snacks",  names(data))
names(data) <- gsub("X.17_MAQ_9..Processed.Meat..sausage.and.meat.sticks.beef.jerky",
                    "Snacked_Cat_ProcessedMeat_Snacks", names(data))
names(data) <- gsub("X.17_MAQ_11..Other..please.specify.hereafter.", "Snacked_Cat_Other_Snacks",  names(data))

# Rename variables (Snacking Amount per category) 
names(data) <- gsub("X.18_SAQ..2b..Snacking.Amount.Sweet.Snacks", "SA_Sweet_Snacks",  names(data))
names(data) <- gsub("X.19_SAQ..2c..Snacking.Amount.Savory.Snacks", "SA_Savory_Snacks",  names(data))
names(data) <- gsub("X.20_SAQ..2d..Snacking.Amount.Fried.Snacks",  "SA_Fried_Snacks",  names(data))
names(data) <- gsub("X.21_SAQ..2e..Snacking.Amount.Dairy.Snacks", "SA_Dairy_Snacks",   names(data))
names(data) <- gsub("X.22_SAQ..2f..Snacking.Amount.Baked.Snacks",  "SA_Baked_Snacks", names(data))
names(data) <- gsub("X.23_SAQ..2g..Snacking.Amount.Dried.Fruits...Nuts", "SA_DriedFruitNuts_Snacks",  names(data))
names(data) <- gsub("X.24_SAQ..2h..Snacking.Amount.Bread.Crackers.Sandwich", "SA_Sandwich_Snacks",  names(data))
names(data) <- gsub("X.25_SAQ..2i..Snacking.Amount.Energy.Granola.Bars","SA_EnergyBars_Snacks",  names(data))
names(data) <- gsub("X.26_SAQ..2j..Snacking.Amount.Processed.Meat", "SA_ProcessedMeat_Snacks", names(data))
names(data) <- gsub("X.29_SAQ..2m..Snacking.Amount.Other.Snacks",   "SA_Other_Snacks",  names(data))

# Rename variables (self-control, stress)
names(data) <- gsub("X.30_VAS..3..Self.Control", "Self_Control", names(data))
names(data) <- gsub("X.31_VAS..4..Stress",  "Stress",   names(data))

# Rename variables FILLERS (assessed only if pp answered 'No' to craving and/or snacking items)
names(data) <- gsub("X.33_VAS..5.1..Energy",  "Energy_level",  names(data))
names(data) <- gsub("X.35_VAS..6.1..Boredom.Intensity",  "Boredom", names(data))

# Rename variables (Craving degree per category) 
names(data) <- gsub("X.15_VAS..1..Craving.Degree.Sweet.Snacks",  "CD_Sweet_Snacks",  names(data))
names(data) <- gsub("X.36_VAS..1..Craving.Degree.Savory.Snacks",  "CD_Savory_Snacks",  names(data))
names(data) <- gsub("X.37_VAS..1..Craving.Degree.Fried.Snacks", "CD_Fried_Snacks",  names(data))
names(data) <- gsub("X.38_VAS..1..Craving.Degree.Dairy.Snacks", "CD_Dairy_Snacks",  names(data))
names(data) <- gsub("X.39_VAS..1..Craving.Degree.Baked.Snacks",  "CD_Baked_Snacks",  names(data))
names(data) <- gsub("X.40_VAS..1..Craving.Degree.Dried.Fruits...Nuts", "CD_DriedFruitNuts_Snacks", names(data))
names(data) <- gsub("X.41_VAS..1..Craving.Degree.Bread.Crackers.Sandwich", "CD_Sandwich_Snacks",  names(data))
names(data) <- gsub("X.42_VAS..1..Craving.Degree.Energy.Granola.Bars", "CD_EnergyBars_Snacks",  names(data))
names(data) <- gsub("X.43_VAS..1..Craving.Degree.Processed.Meat",  "CD_ProcessedMeat_Snacks", names(data))
names(data) <- gsub("X.45_VAS..1..Craving.Degree.Other",  "CD_Other_Snacks", names(data))

# 4. Drop/delete some variables. If needed, go back to the merged raw data. 
# Important: here I dropped info about snack category that was craved and/or eaten. If interested, comment out

cols_to_remove <- c(
  "Participant.Label", "Unanswered.Status",
  "X.1_SAQ.Metadata..1..Craving", "X.2_MAQ.Metadata..1a..Snack",
  "X.3_SAQ.Metadata..1b..Craving.Amount.Sweet.Snacks",
  "X.4_SAQ.Metadata..1c..Craving.Amount.Savory.Snacks",
  "X.5_SAQ.Metadata..1d..Craving.Amount.Fried.Snacks",
  "X.6_SAQ.Metadata..1e..Craving.Amount.Dairy.Snacks",
  "X.7_SAQ.Metadata..1f..Craving.Amount.Baked.Snacks",
  "X.8_SAQ.Metadata..1g..Craving.Amount.Dried.Fruits...Nuts",
  "X.9_SAQ.Metadata..1h..Craving.Amount.Bread.Crackers.Sandwich",
  "X.10_SAQ.Metadata..1i..Craving.Amount.Energy.Granola.Bars",
  "X.11_SAQ.Metadata..1j..Craving.Amount.Processed.Meat",
  "X.13_FFT..1l..Specify.Other.Snacks", "X.13_FFT.Metadata..1l..Specify.Other.Snacks",
  "X.14_SAQ.Metadata..1m..Craving.Amount.Other.Snacks",
  "X.15_VAS.Metadata..1..Craving.Degree.Sweet.Snacks",
  "X.16_SAQ.Metadata..2..Intake", "X.17_MAQ.Metadata..2a..Snack",
  "X.18_SAQ.Metadata..2b..Snacking.Amount.Sweet.Snacks",
  "X.19_SAQ.Metadata..2c..Snacking.Amount.Savory.Snacks",
  "X.20_SAQ.Metadata..2d..Snacking.Amount.Fried.Snacks",
  "X.21_SAQ.Metadata..2e..Snacking.Amount.Dairy.Snacks",
  "X.22_SAQ.Metadata..2f..Snacking.Amount.Baked.Snacks",
  "X.23_SAQ.Metadata..2g..Snacking.Amount.Dried.Fruits...Nuts",
  "X.24_SAQ.Metadata..2h..Snacking.Amount.Bread.Crackers.Sandwich",
  "X.25_SAQ.Metadata..2i..Snacking.Amount.Energy.Granola.Bars",
  "X.26_SAQ.Metadata..2j..Snacking.Amount.Processed.Meat",
  "X.28_FFT.Metadata..2l..Specify.Other.Snacks", "X.28_FFT..2l..Specify.Other.Snacks",
  "X.29_SAQ.Metadata..2m..Snacking.Amount.Other.Snacks",
  "X.30_VAS.Metadata..3..Self.Control", "X.31_VAS.Metadata..4..Stress",
  "X.33_VAS.Metadata..5.1..Energy", "X.35_VAS.Metadata..6.1..Boredom.Intensity",
  "X.36_VAS.Metadata..1..Craving.Degree.Savory.Snacks",
  "X.37_VAS.Metadata..1..Craving.Degree.Fried.Snacks",
  "X.38_VAS.Metadata..1..Craving.Degree.Dairy.Snacks",
  "X.39_VAS.Metadata..1..Craving.Degree.Baked.Snacks",
  "X.40_VAS.Metadata..1..Craving.Degree.Dried.Fruits...Nuts",
  "X.41_VAS.Metadata..1..Craving.Degree.Bread.Crackers.Sandwich",
  "X.42_VAS.Metadata..1..Craving.Degree.Energy.Granola.Bars",
  "X.43_VAS.Metadata..1..Craving.Degree.Processed.Meat",
  "X.45_VAS.Metadata..1..Craving.Degree.Other",
  "Device.ID", "UUID", "Activity.Version", "Prompt.Time",
  "Record.Time", "Status", "Triggering.Logic.ID", "Triggering.Logic.Type",
  "Craved_Cat_Sweet_Snacks", "Craved_Cat_Savory_Snacks", "Craved_Cat_Fried_Snacks", "Craved_Cat_Dairy_Snacks.", "Craved_Cat_Baked_Snacks", "Craved_Cat_DriedFruitNuts_Snacks",
  "Craved_Cat_Sandwick_Snacks", "Craved_Cat_EnergyBars_Snacks", "Craved_Cat_ProcessedMeat_Snacks", "Craved_Cat_Others_Snacks", "Snacked_Cat_Sweet_Snacks.", "Snacked_Cat_Savory_Snacks",
  "Snacked_Cat_Fried_Snacks", "Snacked_Cat_Dairy_Snacks", "Snacked_Cat_Baked_Snacks", "Snacked_Cat_DriedFruitNuts_Snacks", "Snacked_Cat_Sandwich_Snacks", "Snacked_Cat_EnergyBars_Snacks", 
  "Snacked_Cat_ProcessedMeat_Snacks", "Snacked_Cat_Other_Snacks", "Energy_level", "Boredom")
data <- select(data, -any_of(cols_to_remove))

# 5. Create 2 more (separate) variables: Date and Time. Useful later for 'variable_computation' (matlab script)
if ("Date_Time" %in% names(data)) {
  data$Date_Time <- ymd_hms(data$Date_Time, quiet = TRUE)
  data$Date      <- as.Date(data$Date_Time)
  data$Time      <- format(data$Date_Time, "%H:%M:%S")
  
  data <- data %>%
    group_by(ID) %>% mutate(Day_nr   = match(Date, sort(unique(Date))))      %>% ungroup() %>%
    group_by(ID) %>% mutate(Week_nr  = ceiling(Day_nr / 7))                  %>% ungroup() %>%
    group_by(ID, Date) %>% mutate(Timepoint = match(Time, sort(unique(Time)))) %>% ungroup()
}


# 6. Create variable called Prompt (useful later for VAR)  
if ("ID" %in% names(data)) {
  data <- data %>%
    group_by(ID) %>%
    mutate(Prompt = row_number()) %>%
    ungroup()
}


# 7. Replace answer 'more than 5' with '6' in CA and SA (portions) variables  
# Replace 'more than 5' with '6' in both CA and SA variables
ca_vars <- grep("^CA_", names(data), value = TRUE)
sa_vars <- grep("^SA_", names(data), value = TRUE)

data <- data %>%
  mutate(across(all_of(c(ca_vars, sa_vars)), ~ ifelse(. == "more than 5", "6", .)))


################################################
# 8. IMPORTANT: Collapse CD, CA, SA into 3 overall/summary variables (across all snack types/categories).
# The 3 new variables are: CD = craving degree; CA = craving amount; SA = snacking amount
################################################

cd_vars <- grep("^CD_", names(data), value = TRUE)   # 10 craving degree (VAS) variables
ca_vars <- grep("^CA_", names(data), value = TRUE)   # 10 craving amount (portions) variables
sa_vars <- grep("^SA_", names(data), value = TRUE)   # 10 snacking amount (portions) variables

data <- data %>% 
  ## make sure all 30 inputs are numeric
  mutate(across(all_of(c(cd_vars, ca_vars, sa_vars)), ~ as.numeric(trimws(.)))) %>% 
  ## row-wise aggregate
  rowwise() %>% 
  mutate(
    CD = mean(c_across(all_of(cd_vars)), na.rm = TRUE),   # mean of 10 CD_*
    CA = sum (c_across(all_of(ca_vars)), na.rm = TRUE),   # sum  of 10 CA_*
    SA = sum (c_across(all_of(sa_vars)), na.rm = TRUE)    # sum  of 10 SA_*
  ) %>% 
  ungroup() %>% 
  ## drop/delete the original 30 variables (10 CD, 10 CA, 10 SA) 
  select(-all_of(c(cd_vars, ca_vars, sa_vars)))


# 9. IMPORTANT: If pp did not crave and/or snack, score in CD, CA, SA are set to ZERO (instead of NAN or empty cell)

data <- data %>%
  mutate(
    CD = ifelse(`Craved_Y/N`  == "0", 0, CD),
    CA = ifelse(`Craved_Y/N`  == "0", 0, CA),
    SA = ifelse(`Snacked_Y/N` == "",  NA, SA)
  )

# 10. Convert empty values in CD to 0 (if Craved_Y/N = No)
data$CD[is.nan(data$CD)] <- 0

# 10b. Create two new variables: CF and SF (ie, craving frequency and snacking frequency)
data <- data %>%
  mutate(
    CF = ifelse(trimws(`Craved_Y/N`) == "Yes", 1,
                ifelse(trimws(`Craved_Y/N`) == "No", 0, NA)),
    SF = ifelse(trimws(`Snacked_Y/N`) == "Yes", 1,
                ifelse(trimws(`Snacked_Y/N`) == "No", 0, NA))
  )

# 11. Reorder columns/variables in a specified order, see below
final_order <- c(
  "ID", "Date_Time", "Craved_Y/N", "CF", "CD", "CA",
  "Snacked_Y/N", "SF", "SA",
  "Self_Control", "Stress", "Date", "Time", "Day_nr", "Prompt", "Week_nr"
)

# Apply the order set above
data <- data[, c(final_order, setdiff(names(data), final_order))]


# 12. Save the preprocessed/cleaned dataset 
write.xlsx(data, output_file)

cat("Preprocessing complete. Saved to:", output_file, "\n")
