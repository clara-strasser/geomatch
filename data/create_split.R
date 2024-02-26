############################ TRAIN - TEST SPLIT ################################

# Load packages ----------------------------------------------------------------
library(dplyr)

# Load data --------------------------------------------------------------------

# Refugee data
load("data/raw/refugee_data.RData")

# Migrants data
load("data/raw/migrants_data.RData")


# Train - Test Split -----------------------------------------------------------


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


## 3) Order data -------------------------

data <- data %>%
  select(aid, rid, cid, csize,
         everything())

## 4) Correct formats --------------------

data$aid <- as.factor(data$aid)
data$employment_year_arrival <- as.numeric(data$employment_year_arrival)
data$employment_one_year_arrival <- as.numeric(data$employment_one_year_arrival)
data$employment_two_year_arrival <- as.numeric(data$employment_two_year_arrival)

## 5) Complete cases -------------------- 

data <- data[complete.cases(data$employment_year_arrival), ]
data <- data[complete.cases(data$employment_one_year_arrival), ]
data <- data[complete.cases(data$employment_two_year_arrival), ]

## 6) Modify variables -----------------

# employment_one_year_arrival
data <- data %>%
  mutate(employment_one_year_arrival = ifelse(employment_year_arrival==2 | employment_one_year_arrival == 2,2, 1))

# employment_two_year_arrival
data <- data %>%
  mutate(employment_two_year_arrival = ifelse(employment_one_year_arrival==2 | employment_two_year_arrival == 2,2, 1))

# employment_two_year_arrival

## 6) Split data -------------------------
# Lframe - 11.019
# Rframe - 2021

Lframe <- data %>%
  filter(immiyear <= 2016)

Rframe <- data %>%
  filter(immiyear >= 2017)


## 7) Save data ------------------------

save(Lframe, file = "data/processed/Lframe.RData")
save(Rframe, file = "data/processed/Rframe.RData")

















