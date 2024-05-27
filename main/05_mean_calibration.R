######################## Model Performance ####################################
# Mean Calibration

rm(list=ls())


# Load packages ----------------------------------------------------------------

library(predtools)
library(magrittr)
library(dplyr)
library(ggplot2)


# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser"
path_data_final <- file.path(base_path, "geomatch_data")
path_data_output <- file.path(base_path, "geomatch_data", "output", "plots")

# Specification ----------------------------------------------------------------

# Specify sample splits
sample_splits <- list(list(name="refmig_refmig",
                           ref=T,
                           mig=T),
                      list(name="refmig_ref",
                           ref=T,
                           mig=F))


# Specify outcome variables
outcome_variables <- c("employment_one_year_arrival",
                       "employment_two_year_arrival",
                       "employment_three_year_arrival",
                       "employment_four_year_arrival")


# Calibration Plot -------------------------------------------------------------

for (sample_split in 1:length(sample_splits)){
  for (outcome_variable in outcome_variables) {
    
    # 1) Load data  -----------------------
    LRtoOMout <- readRDS(paste0(path_data_final, "/", sample_splits[[sample_split]]$name, "/", outcome_variable, "/", "LRtoOMout.rds"))
    
    
   # 2) Read data -----------------------
    data <- LRtoOMout[[1]]
    
    
    # 3) Modify data --------------------
    data <- data %>%
      select(-1:-((ncol(data)-18))) 
    data <- data %>%
      rename_with(~paste0("pred_", .), matches("^\\d+$"))
    
    
    # 4) Generate overall PRAUC value -----------
    data$pred <- apply(data, 1, function(x) x[paste0("pred_", x["aid"])])
    data <- data %>%
      mutate(pred = as.numeric(pred)) 
    
    # 5) Save data -----------------
    plot_list  <- calibration_plot(data = data, obs = "outcome", pred = "pred")
    plot <- plot_list$calibration_plot 
    ggsave(paste0(path_data_output,"/",sample_splits[[sample_split]]$name, "_", outcome_variable, "_calibration", ".png") , plot)
    
  }
}



