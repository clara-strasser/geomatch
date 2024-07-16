######################## Model Performance ####################################
# Measure Precision - Recall AUC

rm(list=ls())

# Load packages ----------------------------------------------------------------

library(dplyr)
library(mlr3)
library(mlr3measures)
library(gbm)

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser"
path_data_final <- file.path(base_path, "geomatch_data")
path_data_output <- file.path(base_path, "geomatch_data", "output", "tables")

# Set splits -------------------------------------------------------------------

# Sample Split
sample_splits <- list(list(name="refmig_refmig"),
                      list(name="refmig_ref"),
                      list(name="refmig_mig"),
                      list(name="ref_refmig"),
                      list(name="ref_ref"),
                      list(name="ref_mig"))

# Set outcomes -----------------------------------------------------------------

# Outcome variables
outcome_variables <- c("employment_one_year_arrival",
                       "employment_two_year_arrival",
                       "employment_three_year_arrival",
                       "employment_four_year_arrival")
# PRECISION - RECALL -----------------------------------------------------------

# Create table -----------------------------------------------------------------
prauc_results <- data.frame(
  sample_split = character(1),
  outcome_variable = character(1),
  prauc_overall = NA
)

# Add 16 columns ---------------------------------------------------------------
prauc_results[, paste0("prauc_", 1:16)] <- NA



for (sample_split in 1:length(sample_splits)){
    for (outcome_variable in outcome_variables) {
      
      file_path <- paste0(path_data_final, "/", sample_splits[[sample_split]]$name, "/", outcome_variable, "/", "LRtoOMout.rds")
      
      # Check if the file exists
      if (file.exists(file_path)) {
        
      # 1) Load data  -----------------------
        LRtoOMout <- readRDS(file_path)
        
      # 2) Extract data --------------------
      data <- LRtoOMout[[1]]
      
      # 3) Modify data --------------------
      data <- data %>%
        select(-1:-((ncol(data)-18))) 
      data <- data %>%
        mutate(outcome = as.factor(outcome))
      data <- data %>%
        rename_with(~paste0("pred_", .), matches("^\\d+$"))
      
      # 4) Generate overall PRAUC value -----------
      data$pred <- apply(data, 1, function(x) x[paste0("pred_", x["aid"])])
      data <- data %>%
        mutate(pred = as.numeric(pred)) 
      prauc_value_overall <- mlr3measures::prauc(data$outcome, data$pred, "1")
      
      # 4) Generate PRAUC values per state ------------
      prauc_row <- data.frame(
        sample_split = sample_splits[[sample_split]]$name,
        outcome_variable = outcome_variable,
        prauc_overall = prauc_value_overall
      )
      
      for (i in 1:16) {
        
        subset_data <- data[data$aid == i, ]
        pred_col <- paste0("pred_", i)
        
        prauc_value <- mlr3measures::prauc(subset_data$outcome, subset_data[[pred_col]], "1")
        prauc_row[[paste0("prauc_", i)]] <- prauc_value
      }
      
      # Append row to auc_results dataframe
      prauc_results <- rbind(prauc_results, prauc_row)
      
      
      } else {
        
        # Handle the case where the file does not exist
        message(paste("File not found for sample split", sample_splits[[sample_split]]$name, "and outcome variable", outcome_variable))
        
        # You can add additional handling here if needed, such as setting `data` to `NULL` or skipping the iteration.
        next
    }
  }
} 


# Save table R------------------------------------------------------------------
prauc_results <- prauc_results[-1, ]
save(prauc_results, file = paste0(path_data_output, "/prauc_results.RData"))

# Save table Excel -------------------------------------------------------------

# Load existing workbook
wb <- loadWorkbook(paste0(path_data_final,"/", "results.xlsx"))
addWorksheet(wb, "AUC PR")
writeData(wb, "AUC PR", prauc_results)
setColWidths(wb, sheet = "AUC PR", cols = 1:ncol(prauc_results), widths = "auto")
saveWorkbook(wb, paste0(path_data_final,"/","results.xlsx"), overwrite = TRUE)







