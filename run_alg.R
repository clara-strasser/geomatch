########################## GEOMATCH TOOL #####################################
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
# IAB-BAMF-SOEP data
# IAB-SOEP-MIG data

load("data/processed/refugee_data.RData")
load("data/processed/migrants_data.RData")


# Train-Test split -------------------------------------------------------------
# Maybe with function?
# Idea: combine mig and ref till 2016 for train, and from 17 for test


#Lframe <- All working age refugees who arrived till 2016Q4
#Rframe <- All working age free case refugees who arrived 2017Q1


# Main -------------------------------------------------------------------------

## 1) MODELING -----------------------------------------------------------------

### 1.1) Specification ---------------------------------------------------------

# Specify predictors
predictors <- c("immiyear", "sex", "birth_year",
                "birth_month", "free_case", "partner",
                "age_immigration", "corigin", "religious_affiliation",
                "german_speaking", "german_writing", "german_reading",
                "school_degree_med", "school_degree_high", "vocational_training")


# Specify locations 
incl.locs <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")


### 1.2) Gradient Boosted Trees ------------------------------------------------

# Source modeling function
source("src/func_LR_to_OM_boostedtreesCV.R")

# Implement the modeling
# the first object in the output list is combined O and M matrices
LRtoOMout <- func_LR_to_OM_boostedtreesCV_binary(outcome = "employment_one_year_arrival",
                                                 Lframe = Lframe, Rframe = Rframe,
                                                 aid = "aid", rid = "rid",
                                                 cid = "cid", csize = "csize",
                                                 incl.locs = incl.locs, 
                                                 predictors = predictors,
                                                 depth.vec = c(4,5,6),n.trees=1000,
                                                 shrink = 0.01)
# Save list
rds_file_name <- "LRtoOMout_2.rds"
saveRDS(LRtoOMout, file.path("output/", rds_file_name))


## 2) MAPPING ------------------------------------------------------------------

# Split OM into O and M matrices
OM <- LRtoOMout[[1]]
lastvar <- 6

# O: N x P dataframe with refugee identifier/information variables
O <- OM[,1:lastvar]

# M: N x L matrix of refugee-location predicted probabilities of employment
M <- as.matrix(OM[,-(1:lastvar)])
rownames(M) <- O$rid

# Source mapping function 
source("src/func_M_to_Mstar.R")

# Implement mapping
Mstar <- func_M_to_Mstar(mmat = M, cid = O$cid, tfunc = compute_femp_prob)

# Note: case-location assignment constraints can be imposed here by
# setting arbitrarily low values (e.g. -99) for relevant entries in Mstar


## 3) MATCHING -----------------------------------------------------------------

# Set cushion for location capacity constraints
# (1 if each location is to take exact number of cases specified)
Cushion <- 1

# Specificy location capacity slots with named vector
# The following can be used in backtest context:
holder <- (O %>% group_by(aid) %>% summarise(out = length(unique(cid))))
slots <- holder$out
names(slots) <- holder$aid

# Create distance matrix for input into optimal matching procedure
#(see function code for argument and output details)
source("src/func_Mstar_to_D.R")
D <- func_Mstar_to_D(Mstar = Mstar, slots = slots, cushion = Cushion)

# Implement optimal matching via linear sum optimality criterion
library(optmatch)
options(optmatch_max_problem_size = Inf)
optout <- pairmatch(x = D, controls = 1)


# Reformat output (see function code for argument and output details)
source("src/func_optmatch_to_Astar.R")
aids <- unique(colnames(Mstar))
Astar <- func_optmatch_to_Astar(optout = optout, aids = aids, print=0)

# Comparing case assignment numbers (if backtest)
slots
table(Astar$raid)

# Convert case-level assignments to individual-level assignments
#(see function code for argument and output details)
source("src/func_Astar_to_A.R")
A <- func_Astar_to_A(Astar = Astar, O = O, M = M, print=0)

#raid column is optimal assignment
#predprob is predicted probability at optimal assignment
#aid column is original assignment (if backtest)


# Preview results --------------------------------------------------------------

summary(A)
mean(A$outcome)
mean(A$predprob)
(mean(A$predprob)-mean(A$outcome))/mean(A$outcome)

# Save results -----------------------------------------------------------------











