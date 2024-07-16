######################## Model Performance ####################################
# Measure ROC AUC

rm(list=ls())

# Load packages ----------------------------------------------------------------

library(dplyr)
library(mlr3)
library(mlr3measures)
library(gbm)
library(openxlsx)

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser"
path_data_final <- file.path(base_path, "geomatch_data")
path_data_output <- file.path(base_path, "geomatch_data", "output", "tables")

# Set splits -------------------------------------------------------------------

# Sample Split
sample_splits <- list(list(name="ref_ref"),
                      list(name="refmig_ref")
                      )
#sample_splits <- list(list(name="refmig_refmig"),
                     # list(name="refmig_ref"),
                      #list(name="refmig_mig"),
                     # list(name="ref_refmig"),
                     # list(name="ref_ref"),
                     # list(name="ref_mig"))


# Set outcomes -----------------------------------------------------------------

# Outcome variables
outcome_variables <- c("employment_two_year_arrival",
                       "employment_three_year_arrival",
                       "employment_four_year_arrival")



# ROC-AUC  ---------------------------------------------------------------------

# Create table -----------------------------------------------------------------
auc_results <- data.frame(
  sample_split = character(1),
  outcome_variable = character(1),
  auc_overall = NA
)

# Add 16 columns ---------------------------------------------------------------
auc_results[, paste0("auc_", 1:16)] <- NA

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
      
      # 4) Generate overall AUC value -----------
      data$pred <- apply(data, 1, function(x) x[paste0("pred_", x["aid"])])
      data <- data %>%
        mutate(pred = as.numeric(pred)) 
      auc_value_overall <- mlr3measures::auc(data$outcome, data$pred, "1")
      
      # 5) Generate AUC values per state ------------
      auc_row <- data.frame(
        sample_split = sample_splits[[sample_split]]$name,
        outcome_variable = outcome_variable,
        auc_overall = auc_value_overall
      )
      
      for (i in 1:16) {
        
        subset_data <- data[data$aid == i, ]
        pred_col <- paste0("pred_", i)
        
        auc_value <- mlr3measures::auc(subset_data$outcome, subset_data[[pred_col]], "1")
        auc_row[[paste0("auc_", i)]] <- auc_value
      }
      
      
      # Append row to auc_results dataframe
      auc_results <- rbind(auc_results, auc_row)
      
     } else {
       
      # Handle the case where the file does not exist
       message(paste("File not found for sample split", sample_splits[[sample_split]]$name, "and outcome variable", outcome_variable))
       
      # You can add additional handling here if needed, such as setting `data` to `NULL` or skipping the iteration.
       next
    }
  }
}  


# Save table R -----------------------------------------------------------------
auc_results <- auc_results[-1,]
save(auc_results, file = paste0(path_data_output, "/auc_results_updated.RData"))

# Save table Excel -------------------------------------------------------------

# Load existing workbook
wb <- loadWorkbook(paste0(path_data_final,"/", "results.xlsx"))
addWorksheet(wb, "AUC ROC")
writeData(wb, "AUC ROC", auc_results)
setColWidths(wb, sheet = "AUC ROC", cols = 1:ncol(auc_results), widths = "auto")
saveWorkbook(wb, paste0(path_data_final,"/","results.xlsx"), overwrite = TRUE)


# Analysis
auc_results_analysis <- auc_results[,4:19]
apply(auc_results_analysis, 1, mean)
apply(auc_results_analysis, 1, max)


write.csv(auc_results, file = paste0(path_data_final,"/","results.csv"), row.names = FALSE)



