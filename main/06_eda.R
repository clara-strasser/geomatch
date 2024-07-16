############################ EDA ####################################
# Aim: Exploratory Data Analysis on train and test data set

rm(list=ls())

# Load packages ----------------------------------------------------------------
library(foreign)
library(dplyr)
library(stringr)
library(optmatch)
library(parallel)
library(openxlsx)
library(ggplot2)

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser"
path_data_soep <- file.path(base_path, "soep_data", "final")
path_data_geomatch <- file.path(base_path, "geomatch_data", "final")
path_data_final <- file.path(base_path, "geomatch_data")

# Load functions ---------------------------------------------------------------
source("src/func_train_test_split.R")
source("src/func_sample_split.R")
source("src/func_data_to_complete.R")

# Read Data --------------------------------------------------------------------

# Define path
dir <- paste0(path_data_geomatch, "/data_geomatch.RData")

# Split into Lframe and Rframe
data_splits <- train_test_split(dir, train_year = 2015, test_year = 2016)

# Rows: 9.558
Lframe <- data_splits$train
# Rows: 3.319
Rframe <- data_splits$test

# Combine 
data <- rbind(Lframe, Rframe)


# Country of Origin ------------------------------------------------------------

# Subset data
data_corigin <- data %>%
  select(pid, corigin, immiyear, refugee_sample) %>%
  mutate(before16 = ifelse(immiyear <= 2015, 1,0))

# Group data
summary_table <- data_corigin %>%
  group_by(before16, corigin) %>%
  summarize(
    Refugee_Sample_Yes = sum(refugee_sample == 1),
    Refugee_Sample_No = sum(refugee_sample == 0)
  )

# Rename columns to desired format
summary_table <- summary_table %>%
  rename(Country = corigin)

# Group data
summary_table <- data_corigin %>%
  group_by(corigin) %>%
  summarize(
    Refugee_Sample_Yes = sum(refugee_sample == 1),
    Refugee_Sample_No = sum(refugee_sample == 0)
  )

# Rename columns to desired format
summary_table <- summary_table %>%
  rename( Country = corigin)

















