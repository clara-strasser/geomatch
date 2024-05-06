############################ Run Algorithm ####################################
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

# Load functions ---------------------------------------------------------------
source("src/func_LR_to_OM_boostedtreesCV.R")
source("src/read_split_data.R")
source("src/func_M_to_Mstar.R")
source("src/func_Mstar_to_D.R")
source("src/func_optmatch_to_Astar.R")
source("src/func_Astar_to_A.R")


# Specification ----------------------------------------------------------------

# Specify predictors
predictors <- c("immiyear", "sex", "birth_year", "birth_month",
                "refugee_sample", "free_case", "partner",
                "age_immigration", "corigin", "religious_affiliation",
                "german_speaking", "german_writing", "german_reading", 
                "school_degree_med", "school_degree_high", "school_years", "vocational_training")

# Specify locations 
incl.locs <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")

# Empty Table ------------------------------------------------------------------
results <- data.frame(
  Train_Test 
  Rows = c("refmig_refmig", "refmig_ref"),
  Mean_Outcome = numeric(2),
  Mean_Pred = numeric(2),
  Rel_Difference = numeric(2)
)


# Run Algorithm ----------------------------------------------------------------

for (split in c("refmig_refmig", "refmig_ref")) {
  
  
  # 1) Load data  -----------------------
  data <- read_split_data(split)
  Lframe <- data$Lframe
  Rframe <- data$Rframe
  
  
  # 2) Modelling ----------------------
  set.seed(1234)
  LRtoOMout <- func_LR_to_OM_boostedtreesCV_binary(outcome = "employment_one_year_arrival",
                                                     Lframe = Lframe, Rframe = Rframe,
                                                     aid = "aid", rid = "rid",
                                                     cid = "cid", csize = "csize",
                                                     incl.locs = incl.locs, 
                                                     predictors = predictors,
                                                     depth.vec = c(4,5,6),n.trees=1000,
                                                     shrink = 0.01)
  
  # 3) Mapping ----------------------
  OM <- LRtoOMout[[1]]
  lastvar <- 6
  O <- OM[,1:lastvar]
  M <- as.matrix(OM[,-(1:lastvar)])
  rownames(M) <- O$rid
  Mstar <- func_M_to_Mstar(mmat = M, cid = O$cid, tfunc = compute_femp_prob)
  
  
  # 4) Matching ---------------------

  Cushion <- 1
  holder <- (O %>% group_by(aid) %>% summarise(out = length(unique(cid))))
  slots <- holder$out
  names(slots) <- holder$aid
  D <- func_Mstar_to_D(Mstar = Mstar, slots = slots, cushion = Cushion)
  options(optmatch_max_problem_size = Inf)
  optout <- pairmatch(x = D, controls = 1)
  aids <- unique(colnames(Mstar))
  Astar <- func_optmatch_to_Astar(optout = optout, aids = aids, print=0)
  slots
  table(Astar$raid)
  A <- func_Astar_to_A(Astar = Astar, O = O, M = M, print=0)
  
  
  # 5) Preview results ------------------------
  
  # Calculate mean outcome
  mean_outcome <- mean(A$outcome)
  
  # Calculate mean predicted probability
  mean_pred <- mean(A$predprob)
  
  # Calculate relative difference
  rel_difference <- (mean_pred - mean_outcome) / mean_outcome
  
  # Assign results to the table
  result_table[result_table$Rows == split, "Mean_Outcome"] <- mean_outcome
  result_table[result_table$Rows == split, "Mean_Pred"] <- mean_pred
  result_table[result_table$Rows == split, "Rel_Difference"] <- rel_difference
  
  # 6) Save
  
  
  
  
}










