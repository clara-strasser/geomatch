############################## Prepare Data ####################################
# Aim: Prepare SOEP data for GeoMatch
# Input:
# data_soep (was created from IAB-BAMF-SOEP)
# Output:
# data_geomatch

rm(list=ls())

# Load packages ----------------------------------------------------------------
library(dplyr)

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser/"
path_data_soep <- file.path(base_path, "soep_data", "final")
path_data_final <- file.path(base_path, "geomatch_data", "final")

# Load data --------------------------------------------------------------------

# data_soep
# Rows: 13.256
load(file.path(path_data_soep, "data_soep.RData"))

# Rename data ------------------------------------------------------------------

data <- data_soep

# Modify data ------------------------------------------------------------------


## 1) Add variables------------------------
# aid: name of variable specifying actual location assignment
# rid: name of variable specifying individual identifier codes
# cid: name of variable specifying case identifier codes
# csize: name of variable specifying size of case (e.g. # refugees in family)


# Variables: aid and rid
data <- data %>%
  mutate(aid = bula_res, 
         rid = pid)

# Variable: cid
# Individual belongs to a case if both partners assigned to the same state
data <- data %>%
  mutate(cid_help = paste(pmin(pid, parid), pmax(pid, parid), sep = "_"))  %>%
  group_by(cid_help) %>%
  mutate(cid_help = ifelse(same_state == 0 & duplicated(cid_help, fromLast = TRUE), paste(cid_help, "_0", sep = ""), cid_help)) %>%
  mutate(cid = cur_group_id()) %>%
  ungroup() %>%
  select(-cid_help)

# Variable: csize
data <- data %>%
  group_by(cid) %>%
  mutate(csize = n())

## 2) Order data ---------------------------------------------------------------

data <- data %>%
  select(aid, rid, cid, csize,
         everything())


## 3) Correct formats ----------------------------------------------------------

data <- data %>%
  mutate(aid = as.character(aid),
         sex = as.factor(sex),
         free_case = as.factor(free_case),
         corigin = as.factor(corigin),
         refugee_sample = as.factor(refugee_sample),
         religious_affiliation = as.factor(religious_affiliation),
         german_speaking = as.factor(german_speaking),
         german_writing = as.factor(german_writing),
         german_reading = as.factor(german_reading),
         school_degree_low = as.factor(school_degree_low),
         school_degree_med = as.factor(school_degree_med),
         school_degree_high = as.factor(school_degree_high),
         vocational_training = as.factor(vocational_training))

# Save data --------------------------------------------------------------------

save(data, file = paste0(path_data_final,"/data_geomatch.RData"))



