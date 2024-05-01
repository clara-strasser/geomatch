######################## Model Performance ####################################
# Sample: Ref Mig - Ref Mig
# Split: T16 - T17
# Aim: Measure ROC-AUC

rm(list=ls())

# Load packages ----------------------------------------------------------------

library(dplyr)
library(mlr3)
library(mlr3measures)
library(gbm)

# Load data --------------------------------------------------------------------

# Employment one year arrival
LRtoOMout <- readRDS("output/train16_test17/refmig_refmig/employment_one_year/LRtoOMout_2.rds")

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


saveRDS(auc_results, "output/train16_test17/refmig_refmig/model_performance/auc_results.rds")



















