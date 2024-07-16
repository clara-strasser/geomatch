######################## Model Performance ####################################
# Measure ROC AUC

rm(list=ls())

# Load packages ----------------------------------------------------------------

library(dplyr)
library(mlr3)
library(mlr3measures)
library(gbm)

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser"
path_data_final <- file.path(base_path, "geomatch_data")

# Load data --------------------------------------------------------------------
LRtoOMout <- readRDS(paste0(path_data_final, "/", "refmig_ref", "/", "employment_one_year_arrival", "/", "LRtoOMout.rds"))


# ROC-AUC  ---------------------------------------------------------------------

# Create table -----------------------------------------------------------------
auc_results <- data.frame(
  sample_split = character(1),
  outcome_variable = character(1),
  auc_overall = NA
)

# Add 16 columns ---------------------------------------------------------------
auc_results[, paste0("auc_", 1:16)] <- NA

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
    
    # Test with other method
    #data$outcome <- as.numeric(data$outcome)
    #data <- data %>%
      #mutate(outcome = ifelse(outcome == 2, 1, 0))
    #auc_value_overall <- gbm.roc.area(data$outcome, data$pred)
    
    
    # 5) Generate AUC values per state ------------
    auc_row <- data.frame(
      sample_split = "refmig_refmig",
      outcome_variable = "employment_one_year_arrival",
      auc_overall = auc_value_overall
    )
    
    i <- 1
    for (i in 1:16) {
      
      subset_data <- data[data$aid == i, ]
      pred_col <- paste0("pred_", i)
      
      auc_value <- mlr3measures::auc(subset_data$outcome, subset_data[[pred_col]], "1")
      auc_row[[paste0("auc_", i)]] <- auc_value
    }
    
    


