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


# Sample Split
sample_splits <- list(list(name="refmig_refmig",
                           cal_auroc=F),
                      list(name="refmig_ref",
                           cal_auroc=F))

# Create table -----------------------------------------------------------------
auc_results <- data.frame(
  train_test_split = character(1), 
  sample_split = character(1)
)

# Add 16 columns ---------------------------------------------------------------
auc_results[, paste0("auc_", 1:16)] <- NA


# Run  -------------------------------------------------------------------------


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
    
    # 4) Generate AUC values ------------
    auc_row <- data.frame(
      train_test_split = train_test_split,
      sample_split = sample_splits[[sample_split]]$name
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


# Save table -------------------------------------------------------------------
auc_results <- auc_results[-1, ]
save(auc_results, file = "output/tables/auc_results.RData")













