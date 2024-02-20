##
# This file contains the replication code for the study
# entitled "Improving Refugee Integration Through Data-Driven Algorithmic Assignment" by 
# Kirk Bansak, Jeremy Ferwerda, Jens Hainmueller, Andrea Dillon,
# Dominik Hangartner, Duncan Lawrence, and Jeremy Weinstein. To be published in Science.
# The algorithm and code for the study was developed by 
# Kirk Bansak and Jens Hainmueller (both Stanford University).
# The algorithm and code are distributed under the terms of the 
# GNU Affero General Public License, Version 3, 19 November 2007.
# The terms of the license are in a file called LICENSE.txt
# which is deposited in this dataverse (doi:10.7910/DVN/MS8XES).
# The algorithm and code comes with ABSOLUTELY NO WARRANTY.
# Please cite this dataverse and the accompanying study when using this material. 


rm(list=ls())

library(foreign)
library(dplyr)


# Load relevant data ------------------------------------------------------

#The United States refugee registry data was provided to us under a 
#Collaboration Research Agreement with a U.S. resettlement agency. 
#This agreement requires that we do not disclose the individual level data
#or transfer the data to a third party. 
#The Swiss ZEMIS database was provided to us under a data use agreement 
#with the Swiss State Secretariat for Migration (SEM) which requires that 
#we do not disclose the individual level data
#or transfer the data to a third party.
#Researchers interested in accessing the data should contact
#Jens Hainmueller: jhain@stanford.edu.


# Define L (model) and R (prediction/test) matrices -----------------------

#For example, for US backtest:
#Lframe <- All working age refugees who arrived 2011Q1 to 2016Q2
#Rframe <- All working age free case refugees who arrived 2016Q3


# Modeling stage: gradient boosted trees ----------------------------------

# Specify vector of predictor variable names

#for US application:
predictors <- c("free_case","english_speaking","gender",
                "arrival_age","educ_high",
                "nat_burma","nat_iraq","nat_bhutan","nat_somalia",
                "nat_afghanistan","nat_demrepcongo","nat_iran",
                "nat_eritrea","nat_ukraine","nat_syria",
                "nat_sudan","nat_ethiopia","nat_moldova",
                "arrival_year","arrival_month")

#for CH application:
predictors <- c("free_case","gender","marital_status","arrival_age",
                "nat_aethiopien","nat_serbien","nat_kosovo","nat_kongodr",
                "nat_afghanistan","nat_irak","nat_srilanka","nat_eritrea",
                "nat_syrien","nat_somalia","nat_bosnienuherzegowina",
                "nat_angola","nat_tuerkei","nat_russland","nat_iran",
                "nat_mazedonienehjugrep","nat_kamerun","nat_nigeria",
                "nat_armenien","nat_cotedivoire","nat_burundi",
                "nat_chinavolksrepublik","nat_guinea",
                "arrival_year","arrival_month","french_lang",
                "muslim","christian")

# Specify vector of location names to include in model building

#For US backtest:
#incl.locs <- vector of local resettlement location names
#For CH backtest:
#incl.locs <- vector of canton names

# Source modeling function (see function code for argument/output details)
source("Functions/func_LR_to_OM_boostedtreesCV.R")

# Implement the modeling
LRtoOMout <- func_LR_to_OM_boostedtreesCV_binary(outcome = "employed",
                                      Lframe = Lframe, Rframe = Rframe,
                                      aid = "aid", rid = "rid",
                                      cid = "cid", csize = "csize",
                                      incl.locs = incl.locs, 
                                      predictors = predictors,
                                      depth.vec = c(4,5,6),n.trees=1000,
                                      shrink = 0.01)
#the first object in the output list is combined O and M matrices


# Mapping stage -----------------------------------------------------------

# Split OM into O and M matrices
OM <- LRtoOMout[[1]]
lastvar <- 6

# O: N x P dataframe with refugee identifier/information variables
O <- OM[,1:lastvar]

# M: N x L matrix of refugee-location predicted probabilities of employment
M <- as.matrix(OM[,-(1:lastvar)])
rownames(M) <- O$rid

# Source mapping function (see function code for argument/output details)
source("Functions/func_M_to_Mstar.R")

# Implement mapping
Mstar <- func_M_to_Mstar(mmat = M, cid = O$cid, tfunc = compute_femp_prob)

# Note: case-location assignment constraints can be imposed here by
# setting arbitrarily low values (e.g. -99) for relevant entries in Mstar


# Matching Stage ----------------------------------------------------------

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
source("Functions/func_Mstar_to_D.R")
D <- func_Mstar_to_D(Mstar = Mstar, slots = slots, cushion = Cushion)

# Implement optimal matching via linear sum optimality criterion
library(optmatch)
options(optmatch_max_problem_size = Inf)
optout <- pairmatch(x = D, controls = 1)

# Reformat output (see function code for argument and output details)
source("Functions/func_optmatch_to_Astar.R")
aids <- unique(colnames(Mstar))
Astar <- func_optmatch_to_Astar(optout = optout, aids = aids, print=0)

# Comparing case assignment numbers (if backtest)
slots
table(Astar$raid)

# Convert case-level assignments to individual-level assignments
#(see function code for argument and output details)
source("Functions/func_Astar_to_A.R")
A <- func_Astar_to_A(Astar = Astar, O = O, M = M, print=0)

#raid column is optimal assignment
#predprob is predicted probability at optimal assignment
#aid column is original assignment (if backtest)


# Preview results ---------------------------------------------------------

summary(A)
mean(A$outcome)
mean(A$predprob)
(mean(A$predprob)-mean(A$outcome))/mean(A$outcome)
