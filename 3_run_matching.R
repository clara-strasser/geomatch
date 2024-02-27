########################## MATCHING #####################################
# References:
# Bansak, K., & Hainmueller, J. (2017). 
# Replication Code for: Improving Refugee Integration Through Data-Driven Algorithmic Assignment.
# Harvard Dataverse. https://doi.org/10.7910/DVN/MS8XES

rm(list=ls())


# Load packages ----------------------------------------------------------------
library(foreign)
library(dplyr)
library(stringr)


# Load mapping results --------------------------------------------------------

# Employment year arrival
load("output/employment_year/Mstar.RData")

# Employment one year arrival
#load("output/employment_one_year/Mstar.RData")

# Employment two year arrival
#load("output/employment_two_year/Mstar.RData")


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











