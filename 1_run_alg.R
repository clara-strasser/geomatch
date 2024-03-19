########################## MODELING #####################################
# References:
# Bansak, K., & Hainmueller, J. (2017). 
# Replication Code for: Improving Refugee Integration Through Data-Driven Algorithmic Assignment.
# Harvard Dataverse. https://doi.org/10.7910/DVN/MS8XES

rm(list=ls())


# Load packages ----------------------------------------------------------------
library(foreign)
library(dplyr)
library(stringr)



# Load data --------------------------------------------------------------------
# Lframe
# Rframe
load("data/processed/Lframe.RData")
load("data/processed/Rframe.RData")


# 1) Main ----------------------------------------------------------------------

## 1.1) Specification ----------------------------------------------------------

# Specify predictors
predictors <- c("immiyear", "sex", "birth_year",
                "refugee_sample","birth_month", "free_case", "partner",
                "age_immigration", "corigin", "religious_affiliation",
                "german_speaking", "german_writing", "german_reading", 
                "school_degree_med", "school_degree_high", "school_years", "vocational_training")

#predictors <- c("immiyear", "sex", "birth_year",
                #"birth_month", "free_case", "partner_no",
                #"partner_married", "partner_cohabitation",
                #"religious_affiliation_catholic", "religious_affiliation_protestant", "religious_affiliation_muslim",
                #"religious_affiliation_atheist", "corigin",
                #"age_immigration",  "german_speaking", "german_writing", "german_reading",
                #"school_degree_med", "school_degree_high", "vocational_training")

# Specify locations 
incl.locs <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")


## 1.2) Gradient Boosted Trees ------------------------------------------------

# Source: Modeling function
source("src/func_LR_to_OM_boostedtreesCV.R")

# Implement: Modeling
set.seed(1234)

# Employment year arrival
LRtoOMout <- func_LR_to_OM_boostedtreesCV_binary(outcome = "employment_year_arrival",
                                                   Lframe = Lframe, Rframe = Rframe,
                                                   aid = "aid", rid = "rid",
                                                   cid = "cid", csize = "csize",
                                                   incl.locs = incl.locs, 
                                                   predictors = predictors,
                                                   depth.vec = c(4,5,6),n.trees=1000,
                                                   shrink = 0.01)

# Employment one year arrival
LRtoOMout_2 <- func_LR_to_OM_boostedtreesCV_binary(outcome = "employment_one_year_arrival",
                                                   Lframe = Lframe, Rframe = Rframe,
                                                   aid = "aid", rid = "rid",
                                                   cid = "cid", csize = "csize",
                                                   incl.locs = incl.locs, 
                                                   predictors = predictors,
                                                   depth.vec = c(4,5,6),n.trees=1000,
                                                   shrink = 0.01)

# Employment two year arrival
LRtoOMout_3 <- func_LR_to_OM_boostedtreesCV_binary(outcome = "employment_two_year_arrival",
                                                 Lframe = Lframe, Rframe = Rframe,
                                                 aid = "aid", rid = "rid",
                                                 cid = "cid", csize = "csize",
                                                 incl.locs = incl.locs, 
                                                 predictors = predictors,
                                                 depth.vec = c(4,5,6),n.trees=1000,
                                                 shrink = 0.01)

# Save -------------------------------------------------------------------------

#rds_file_name <- "LRtoOMout_3_dummy.rds"

# Employment year arrival
#saveRDS(LRtoOMout, file.path("output/employment_year/", "LRtoOMout.rds"))


# Employment one year arrival
#saveRDS(LRtoOMout_2, file.path("output/employment_one_year/", "LRtoOMout_2.rds"))


# Employment two year arrival
#saveRDS(LRtoOMout_3, file.path("output/employment_two_year/", "LRtoOMout_3.rds"))















