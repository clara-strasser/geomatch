######################### FINAL DATA - DESCRIPTIVES ############################

# Load packages ----------------------------------------------------------------
library(foreign)
library(dplyr)
library(stringr)
library(ggplot2)
library(openxlsx)
library(xtable)

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser"
path_data_soep <- file.path(base_path, "soep_data", "final")
path_data_geomatch <- file.path(base_path, "geomatch_data", "final")
path_data_final <- file.path(base_path, "geomatch_data")

# Load functions ---------------------------------------------------------------
source("src/func_train_test_split.R")

# Load data ---------------------------------------------------------------
load(paste0(path_data_geomatch, "/bula/data_bula.RData"))
data_bula <- data

# Train - Test Split -----------------------------------------------------------

# Define path
dir <- paste0(path_data_geomatch, "/data_geomatch.RData")

# Split into Lframe and Rframe
data_splits <- train_test_split(dir, train_year = 2015, test_year = 2016)

# Rows: 9.558
Lframe <- data_splits$train
# Rows: 3.319
Rframe <- data_splits$test


# Lframe Subset
# Rows: 6.807
Lframe <- Lframe %>%
  subset(refugee_sample == 1)

# Rframe Subset
# Rows: 1.526
Rframe <- Rframe %>%
  subset(refugee_sample == 1)

# Train Data  ------------------------------------------------------------------

# 1) Immigration Year and Observations

# Plot
ggplot(Lframe, aes(x = factor(immiyear), )) +
  geom_bar(fill = "#66A182") +
  scale_y_continuous(limits = c(0, 5000)) +
  labs(title = "Number of Observations by Immigration Year",
       x = "Immigration Year",
       y = "Number of Observations") +
  theme_minimal()

# Table
summ_table <- as.data.frame(table(Lframe$immiyear))
colnames(summ_table) <- c("Immigration Year", "Count")
summ_table <- summ_table %>%
  arrange(desc(Count)) 
latex_table <- xtable(summ_table, caption = "Number of Observations by Immigration Year")
print(latex_table, include.rownames = FALSE)


# 2) Federal State and Employment

# Join federal state name
Lframe <- Lframe %>%
  left_join(data_bula, by = c("bula_res" = "id_soep"))

# Create table
summary_table <- Lframe %>%
  group_by(name) %>%
  summarize(
    Observations = as.integer(n()),
    Employment = sum(employment_two_year_arrival == 2, na.rm = TRUE)
  ) %>%
  rename(State = name) %>%
  mutate(perc = (Employment/Observations)*100) %>%
  arrange(desc(Observations)) 
latex_table <- xtable(summary_table)
print(latex_table, include.rownames = FALSE)

# Test Data  -------------------------------------------------------------------


# 1) Immigration Year and Observations
summ_table <- as.data.frame(table(Rframe$immiyear))
colnames(summ_table) <- c("Immigration Year", "Count")
summ_table <- summ_table %>%
  arrange(desc(Count)) 
latex_table <- xtable(summ_table, caption = "Number of Observations by Immigration Year")
print(latex_table, include.rownames = FALSE)


# 2) Federal State and Employment

# Join federal state name
Rframe <- Rframe %>%
  left_join(data_bula, by = c("bula_res" = "id_soep"))

# Create table
summary_table <- Rframe %>%
  group_by(name) %>%
  summarize(
    Observations = as.integer(n()),
    Employment = sum(employment_two_year_arrival == 2, na.rm = TRUE)
  ) %>%
  rename(State = name) %>%
  mutate(perc = (Employment/Observations)*100) %>%
  arrange(desc(Observations)) 
latex_table <- xtable(summary_table)
print(latex_table, include.rownames = FALSE)




