############################ TRAIN - TEST SPLIT ################################
# Sample: Ref Mig - Ref Mig
# Split: T15 - T16
# Aim: Modify data and create data split (training and test data set)


# Load packages ----------------------------------------------------------------
library(dplyr)
library(fastDummies)

# Load data --------------------------------------------------------------------

# Refugee data
load("data/raw/refugee_data.RData")

# Migrants data
load("data/raw/migrants_data.RData")


# Modify data -----------------------------------------------------------


## 1) Combine data ------------------------
# Input:
# refugee_data - 8.309 rows
# migrants_data - 4.732 rows
# Output:
# data - 13.041 rows

data <- rbind(migrants_data, refugee_data)

## 2) Add variables------------------------
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
  mutate(same_state = ifelse(length(unique(bula_res)) == 1, 1, 0)) %>%
  mutate(cid_help = ifelse(same_state == 0 & duplicated(cid_help, fromLast = TRUE), paste(cid_help, "_0", sep = ""), cid_help)) %>%
  mutate(cid = cur_group_id()) %>%
  ungroup() %>%
  select(-cid_help)

# Variable: csize
data <- data %>%
  group_by(cid) %>%
  mutate(csize = n())

# Variable: free_case
# Free case if:
# 1. No partner
# 2. Partner but same_state_new = 0
# 3. Partner not in data set
data <- data %>%
  mutate(free_case = ifelse(parid == -2 | same_state == 0 | (!parid %in% pid & parid != -2) , 1, 0))

# refugee and migrants dummy
# refugees: 17, 18, 19, 24
# migrants: 15, 16, 25, 26
data <- data %>%
  mutate(refugee_sample = ifelse(psample %in% c(17,18,19,24),1,0))


## 3) Order data -------------------------

data <- data %>%
  select(aid, rid, cid, csize,
         everything())

## 4) Correct formats --------------------

data <- data %>%
  mutate(aid = as.character(aid),
         employment_year_arrival = as.numeric(employment_year_arrival),
         employment_one_year_arrival = as.numeric(employment_one_year_arrival),
         employment_two_year_arrival = as.numeric(employment_two_year_arrival))

## 5) Complete cases -------------------- 


###########

Lframe <- Lframe %>%
  filter(!is.na(employment_year_arrival))

Rframe <- Rframe %>%
  filter(!is.na(employment_year_arrival))

###########

Lframe <- Lframe %>%
  filter(!is.na(employment_one_year_arrival))

Rframe <- Rframe %>%
  filter(!is.na(employment_one_year_arrival))

###########

Lframe <- Lframe %>%
  filter(!is.na(employment_two_year_arrival))

Rframe <- Rframe %>%
  filter(!is.na(employment_two_year_arrival))





data <- data %>%
  filter(!is.na(employment_year_arrival)) %>%
  filter(!is.na(employment_one_year_arrival)) %>%
  filter(!is.na(employment_two_year_arrival))

## 6) Modify variables -----------------

# employment_one_year_arrival
data <- data %>%
  mutate(employment_one_year_arrival = ifelse(employment_year_arrival==2 | employment_one_year_arrival == 2,2, 1))

# employment_two_year_arrival
data <- data %>%
  mutate(employment_two_year_arrival = ifelse(employment_one_year_arrival==2 | employment_two_year_arrival == 2,2, 1))

# school_years
data$school_years <- ifelse(data$school_years < 0, NA, data$school_years)


## 7) Remove Missings ----------------

# Get missings 
#colSums(is.na(data))

# Remove missings
#na_columns <- c("religious_affiliation", "german_speaking", "german_writing",
                #"german_reading", "vocational_training")

#data <- data[complete.cases(data[, na_columns]), ]

## 8) One-Hot coding ---------------

# Choose variables and code 

# partner
#data <- dummy_cols(data, select_columns = c("partner"))

# corigin
#data <- dummy_cols(data, select_columns = c("corigin"))

# religious affiliation
#data <- dummy_cols(data, select_columns = c("religious_affiliation"))

# as df
#data <- as.data.frame(data)

## 9) Subset -----------
# Keep only working age population: 18-67
data <- data %>%
  filter(age_immigration >= 18 & age_immigration <= 67)



# Train - Test Split -----------------------------------------------------------


## 1) Split data -------------------------
# Lframe - 9.337
# Rframe - 3.654

# Lframe
Lframe <- data %>%
  filter(immiyear <= 2015)

# Rframe
Rframe <- data %>%
  filter(immiyear >= 2016)

## 2) Handle cases -----------------------
# Keep only cases that are both in Rframe
# If a family member of a case is already in Germany
# than the person in Rframe s assigned to that location.
# Remove that person from Rframe.

# ID
Rframe <- Rframe %>%
  mutate(parid_in_id = ifelse(free_case == 0 & !parid %in% rid & same_state == 1, 1,0))

# Keep only that cases
Rframe <- Rframe %>%
  filter(!parid_in_id == 1)


# Save data ------------------------

# Lframe: 9.337
save(Lframe, file = "data/train15_test16/refmig_refmig/Lframe.RData")

# Rframe: 3.481
save(Rframe, file = "data/train15_test16/refmig_refmig/Rframe.RData")

















