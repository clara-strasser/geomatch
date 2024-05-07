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

# Create table ------------------------------------------------------------------
roc_auc <- data.frame(
  train_test_split = character(1), 
  sample_split = character(1),
  mean_outcome = numeric(1),
  mean_pred = numeric(1),
  rel_difference = numeric(1),
  n = numeric(1),
  refugees = numeric(1)
)







# Extract data -----------------------------------------------------------------

data <- LRtoOMout[[1]]


# Modify data ------------------------------------------------------------------

## Keep selected columns
data <- data %>%
  select(-1:-((ncol(data)-18))) 

## Create true columns

data <- data %>%
  mutate(true_1 = NA, true_2 = NA, true_3 = NA, true_4 = NA, 
         true_5 = NA, true_6 = NA, true_7 = NA, true_8 = NA, 
         true_9 = NA, true_10 = NA, true_11 = NA, true_12 = NA, 
         true_13 = NA, true_14 = NA, true_15 = NA, true_16 = NA)

## Fill true columns

data <- data %>%
  mutate(across(starts_with("true_"), ~ifelse(as.numeric(sub("true_", "", cur_column())) == aid, outcome, 0)))

## Convert to factor
data <- data %>%
  mutate_at(vars(starts_with("true_")), as.factor)


## Rename prediction columns
data <- data %>%
  rename_with(~paste0("pred_", .), matches("^\\d+$"))


# ROC-AUC ----------------------------------------------------------------------

# Create empty table for AUC results
auc_results <- data.frame(matrix(ncol = 16, nrow = 1))

# Generate AUC values
for (i in 1:16) {
  true_col <- paste0("true_", i)
  pred_col <- paste0("pred_", i)
  
  auc_results[1, i] <- mlr3measures::auc(data[[true_col]], data[[pred_col]], "1")
}

# Rename table
colnames(auc_results) <- 1:16


# Save table ------------------------------------------------------------------


saveRDS(auc_results, "output/refmig_ref/model_performance/auc_results.rds")














