############################ TRAIN - TEST SPLIT ################################

# Load packages ----------------------------------------------------------------
library(dplyr)

# Load data --------------------------------------------------------------------



# Train - Test Split -----------------------------------------------------------


## 1) Combine data -------------------------
# Input:
# refugee_data - 8.309 rows
# migrants_data - 4.732 rows
# Output:
# data - 13.041 rows

data <- rbind(migrants_data, refugee_data)

## 2) Add variables-------------------------
# aid: name of variable specifying actual location assignment
# rid: name of variable specifying individual identifier codes
# cid: name of variable specifying case identifier codes
# csize: name of variable specifying size of case (e.g. # refugees in family)

# aid and rid
data <- data %>%
  mutate(aid = bula_res, 
         rid = pid)

# cid
data <- data %>%
  mutate(cid_help = paste(pmin(pid, parid), pmax(pid, parid), sep = "_")) %>%
  group_by(cid_help) %>%
  mutate(cid = cur_group_id()) %>%
  ungroup() %>%
  select(-cid_help)

# csize
data <- data %>%
  group_by(cid) %>%
  mutate(csize = n())

# order
data <- data %>%
  select(aid, rid, cid, csize,
         everything())

# format
# Change to correct format:
data$aid <- as.factor(data$aid)
data$employment_year_arrival <- as.numeric(data$employment_year_arrival)

# remove NA
data <- data[complete.cases(data$employment_year_arrival), ]

## 3) Split data ---------------------------
# Lframe - 11.020
# Rframe - 2021

Lframe <- data %>%
  filter(immiyear <= 2016)

Rframe <- data %>%
  filter(immiyear >= 2017)


## 4)  ------------------------





















