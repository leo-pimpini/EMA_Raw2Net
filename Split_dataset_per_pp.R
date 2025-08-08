
# The script created one file per pp based on ID
# Adapted for EMA Monitoring study. Input data = cleaned merged data
# Author: Leo Pimpini, June 13th 2025

library(openxlsx2)

# 1. Set input and output paths
in_file <- "C:/Users/leonardo.pimpini/Desktop/Cleaned_Data_EMA_Monitoring/Step2_Cleaned_Data/Cleaned_raw_data_Low_F.xlsx"
out_dir <- "C:/Users/leonardo.pimpini/Desktop/Cleaned_Data_EMA_Monitoring/Step3_ReadyforAnalysis/Per_pp/Low"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 2. Read entire first sheet as-is
wb_in      <- wb_load(in_file)               # load source workbook
sheet_name <- wb_in$get_sheet_names()[1]     # first sheet
df         <- wb_to_df(wb_in, sheet = sheet_name, col_names = TRUE)

# 3. Split & write 
ids      <- unique(df[[1]])                  # first column = ID
n_files  <- 0

for (id in ids) {
  part <- df[df[[1]] == id, , drop = FALSE]  # rows for this ID
  if (nrow(part)) {
    wb_out <- wb_workbook()                  # new empty workbook
    wb_out$add_worksheet("Sheet1")           # add a sheet
    wb_out$add_data("Sheet1", x = part)      # write data
    wb_out$save(file.path(out_dir, paste0(id, ".xlsx")),
                overwrite = TRUE)            # save file
    n_files <- n_files + 1
  }
}

cat("The script is done. Tot nr of files created = ", n_files, "\n")
