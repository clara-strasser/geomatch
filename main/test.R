######################## Model Performance ####################################
# Measure ROC-AUC

rm(list=ls())

# Load packages ----------------------------------------------------------------

library(dplyr)
library(mlr3)
library(mlr3measures)
library(gbm)

# Set directory ----------------------------------------------------------------
root_dir <- "output/"

# Set splits -------------------------------------------------------------------

# Train - Test Split
train_test_splits <- list("train15_test16","train16_test17")
#train_test_splits <- list("train15_test16")


# Sample Split
sample_splits <- list(list(name="refmig_refmig",
                           cal_auroc=F),
                      list(name="refmig_ref",
                           cal_auroc=F))
#sample_splits <- list(list(name="refmig_ref",
#cal_auroc=F))

# Set outcomes -----------------------------------------------------------------

# Outcome variables
outcome_variables <- list("LRtoOMout.rds", "LRtoOMout2.rds")

# ROC-AUC  ---------------------------------------------------------------------

# Create table -----------------------------------------------------------------
auc_results <- data.frame(
  train_test_split = character(1), 
  sample_split = character(1),
  outcome_variable = character(1),
  auc_overall = NA
)

# Add 16 columns ---------------------------------------------------------------
auc_results[, paste0("auc_", 1:16)] <- NA

for (outcome_variable in outcome_variables) {
  for (train_test_split in train_test_splits){
    for (sample_split in 1:length(sample_splits)){
    
    # 1) Load data  -----------------------
    LRtoOMout <- readRDS(paste0(root_dir, train_test_split, "/", sample_splits[[sample_split]]$name, "/", outcome_variable))
    
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
      train_test_split = train_test_split,
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
    }
  }
}  


# PRECISION - RECALL -----------------------------------------------------------

# Create table -----------------------------------------------------------------
prauc_results <- data.frame(
  train_test_split = character(1), 
  sample_split = character(1),
  prauc_overall = NA
)

# Add 16 columns ---------------------------------------------------------------
prauc_results[, paste0("prauc_", 1:16)] <- NA


for (train_test_split in train_test_splits){
  for (sample_split in 1:length(sample_splits)){
    
    # 1) Load data  -----------------------
    LRtoOMout <- readRDS(paste0(root_dir, train_test_split, "/", sample_splits[[sample_split]]$name, "/", "LRtoOMout.rds"))
    
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
      train_test_split = train_test_split,
      sample_split = sample_splits[[sample_split]]$name,
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
    
  }
}  


# Save table -------------------------------------------------------------------
auc_results <- auc_results[-1, ]
save(auc_results, file = "output/tables/auc_results.RData")

prauc_results <- prauc_results[-1, ]
save(prauc_results, file = "output/tables/prauc_results.RData")









