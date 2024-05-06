########################## MAPPING and MATCHING ################################
# References:
# Bansak, K., & Hainmueller, J. (2017). 
# Replication Code for: Improving Refugee Integration Through Data-Driven Algorithmic Assignment.
# Harvard Dataverse. https://doi.org/10.7910/DVN/MS8XES

rm(list=ls())


# Load packages ----------------------------------------------------------------
library(foreign)
library(dplyr)
library(stringr)
library(optmatch)

# Load modeling results --------------------------------------------------------

# Employment year arrival
#LRtoOMout <- readRDS("output/employment_year/LRtoOMout.rds")

# Employment one year arrival
LRtoOMout <- readRDS("output/train15_test16/refmig_ref/employment_one_year/LRtoOMout_2.rds")

# Employment two year arrival
#LRtoOMout <- readRDS("output/refmig_refmig/employment_two_year/LRtoOMout_3.rds")



# 2) MAPPING ------------------------------------------------------------------
# Note: case-location assignment constraints can be imposed here by
# setting arbitrarily low values (e.g. -99) for relevant entries in Mstar


## 2.1) Source mapping function ------------------
source("src/func_M_to_Mstar.R")


## 2.2) Split OM into O and M matrices -----------
OM <- LRtoOMout[[1]]
lastvar <- 6

# O: N x P dataframe with refugee identifier/information variables
O <- OM[,1:lastvar]

# M: N x L matrix of refugee-location predicted probabilities of employment
M <- as.matrix(OM[,-(1:lastvar)])
rownames(M) <- O$rid

## 2.3) Implement mapping -------------
Mstar <- func_M_to_Mstar(mmat = M, cid = O$cid, tfunc = compute_femp_prob)



# 3) MATCHING ------------------------------------------------------------------

## 3.1) Source matching functions ----------------------
source("src/func_Mstar_to_D.R")
source("src/func_optmatch_to_Astar.R")
source("src/func_Astar_to_A.R")

## 3.2) Mapping ----------------------


# Set cushion for location capacity constraints
# Note: 1 if each location is to take exact number of cases specified
Cushion <- 1

# Specify location capacity slots with named vector
# The following can be used in backtest context
holder <- (O %>% group_by(aid) %>% summarise(out = length(unique(cid))))
slots <- holder$out
names(slots) <- holder$aid

# Create distance matrix for input into optimal matching procedure
D <- func_Mstar_to_D(Mstar = Mstar, slots = slots, cushion = Cushion)

# Implement optimal matching via linear sum optimality criterion
options(optmatch_max_problem_size = Inf)
optout <- pairmatch(x = D, controls = 1)


# Reformat output (see function code for argument and output details)
aids <- unique(colnames(Mstar))
Astar <- func_optmatch_to_Astar(optout = optout, aids = aids, print=0)

# Comparing case assignment numbers (if backtest)
slots
table(Astar$raid)

# Convert case-level assignments to individual-level assignments
A <- func_Astar_to_A(Astar = Astar, O = O, M = M, print=0)

# raid column is optimal assignment
# predprob is predicted probability at optimal assignment
# aid column is original assignment (if backtest)


## 3.3) Preview results ------------------------

# Mean outcome
mean(A$outcome)

# Mean predicted probability
mean(A$predprob)

# Relative difference between predicted outcome and true outcome
(mean(A$predprob)-mean(A$outcome))/mean(A$outcome)


# Save -------------------------------------------------------------------------


# Employment year arrival
#save(A, file = "output/employment_year/A.RData")

# Employment one year arrival
save(A, file = "output/refmig_ref/employment_one_year/A.RData")

# Employment two year arrival
#save(A, file = "output/refmig_refmig/employment_two_year/A.RData")



