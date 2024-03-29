############################ TRAIN - TEST SPLIT ################################

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
# Lframe - 10974
# Rframe - 2017

# Lframe
Lframe <- data %>%
  filter(immiyear <= 2016)

# Rframe
Rframe <- data %>%
  filter(immiyear >= 2017)

## 2) Handle cases -----------------------
# Keep only cases that are both in Rframe
# If a family member of a case is already in Germany
# than the person in Rframe s assigned to that location.
# Remove that person from Rframe.

# ID
Rframe <- Rframe %>%
  mutate(parid_in_id = ifelse(free_case == "no" & !parid %in% rid, 2,1))

# Keep only that cases
Rframe <- Rframe %>%
  filter(!parid_in_id == 2)


# Save data ------------------------

# Lframe: 10.974
save(Lframe, file = "data/processed/Lframe.RData")

# Rframe: 1.194
save(Rframe, file = "data/processed/Rframe.RData")

















