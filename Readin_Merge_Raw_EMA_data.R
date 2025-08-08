# This script reads in raw EMA data (using the correct delimiter/file layout) and merges files from diff batches into one file
# Adapted for EMA monitoring study 
# Author: Leo Pimpini, June 12th 2025

library(readxl)
library(dplyr)
library(rstudioapi)
library(openxlsx)

####################################
###################################
# Set path to input raw data. Change termination for Low/Medium/High_F_Raw
input_dir <- "C:/Users/leonardo.pimpini/Desktop/Raw_data_EMA_Monitoring/Medium_F_Raw"
####################################
####################################


# Extract batch label from folder name
batch_label <- basename(input_dir)  # Low_F_Raw, Medium_F_Raw, High_F_Raw
batch_suffix <- gsub("_Raw$", "", batch_label)  # Low_F, Medium_F, High_F

# Get path of the current script (RStudio only)
script_path <- dirname(rstudioapi::getSourceEditorContext()$path)

# Dynamically set output Excel file path based on folder name
output_file_xlsx <- file.path(script_path, paste0("Merged_raw_data_", batch_suffix, ".xlsx"))

# List all files in the folder
files <- list.files(input_dir, full.names = TRUE)

# Print files found
cat("Files found:\n")
print(files)

# Function to read files and convert all columns to character
read_raw <- function(file) {
  if (grepl("\\.csv$", file, ignore.case = TRUE)) {
    df <- read.csv(file, stringsAsFactors = FALSE, fileEncoding = "UTF-8", colClasses = "character")
    names(df) <- make.names(names(df), unique = TRUE)
    return(df)
  } else if (grepl("\\.(xlsx|xls)$", file, ignore.case = TRUE)) {
    df <- read_excel(file, col_types = "text")
    names(df) <- make.names(names(df), unique = TRUE)
    return(df)
  } else {
    NULL
  }
}

# Read and store supported files
raw_list <- lapply(files, read_raw)
raw_list <- raw_list[!sapply(raw_list, is.null)]

# Print number of rows in each file
for (i in seq_along(raw_list)) {
  cat("Rows in file", files[i], ":", nrow(raw_list[[i]]), "\n")
}

# Merge all data frames (all columns are character)
merged_data <- bind_rows(raw_list)

# Print total rows
cat("Total rows in merged data:", nrow(merged_data), "\n")

# Save the merged data to Excel only
write.xlsx(merged_data, output_file_xlsx)

# Completion message
cat("The script is complete. Output file is saved to:\n",
    "Excel: ", output_file_xlsx, "\n")
