############################ Run Algorithm ####################################
# Aim: Run GeoMatch algorithm
# References:
# Bansak, K., & Hainmueller, J. (2018). 
# Replication Code for: Improving Refugee Integration Through Data-Driven Algorithmic Assignment.
# Harvard Dataverse. https://doi.org/10.7910/DVN/MS8XES
# Input:
# data_geomatch (was created from IAB-BAMF-SOEP for GeoMatch)
# Output:
# For each train-test split 
# AND
# For each sample split
# 1.) Employment 1 year:  Predictions + Matching results
# 2.) Employment 2 years: Predictions + Matching results
# 3.) Employment 3 years: Predictions + Matching results
# 4.) Employment 4 years: Predictions + Matching results

rm(list=ls())

# Load packages ----------------------------------------------------------------
library(foreign)
library(dplyr)
library(stringr)
library(optmatch)
library(parallel)


# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser"
path_data_soep <- file.path(base_path, "soep_data", "final")
path_data_geomatch <- file.path(base_path, "geomatch_data", "final")
path_data_final <- file.path(base_path, "geomatch_data")

# Load functions ---------------------------------------------------------------
source("src/func_train_test_split.R")
source("src/func_sample_split.R")
source("src/func_data_to_complete.R")
source("src/func_LR_to_OM_boostedtreesCV.R")
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
                "school_degree_low", "school_degree_med", "school_degree_high",
                "school_years", "vocational_training")

# Specify locations 
incl.locs <- c("1", "2", "3", 
               "4", "5", "6",
               "7", "8", "9", 
               "10", "11", "12",
               "13", "14", "15", 
               "16")


# Specify empty tables
results <- data.frame(
  sample_split = character(1),
  outcome_variable = character(1),
  mean_outcome = numeric(1),
  mean_pred = numeric(1),
  rel_difference = numeric(1),
  n = numeric(1),
  refugees = numeric(1)
)


# Specify sample splits
sample_splits <- list(list(name="refmig_refmig",
                           ref=T,
                           mig=T),
                      list(name="refmig_ref",
                           ref=T,
                           mig=F))


# Specify outcome variables
outcome_variables <- c("employment_one_year_arrival",
                       "employment_two_year_arrival",
                       "employment_three_year_arrival",
                       "employment_four_year_arrival")

# Train - Test Split -----------------------------------------------------------

# Define path
dir <- paste0(path_data_geomatch, "/data_geomatch.RData")

# Split into Lframe and Rframe
data_splits <- train_test_split(dir, train_year = 2015, test_year = 2016)

# Rows: 9.558
Lframe <- data_splits$train
# Rows: 3.319
Rframe <- data_splits$test



# Main -------------------------------------------------------------------------

# Specify seed
set.seed(1234)

for (sample_split in 1:length(sample_splits)){
  for (outcome_variable in outcome_variables) {
  
  # 1) Subset Test ----------------------
  Rframe_subset <- test_sample_split(Rframe,
                         ref = sample_splits[[sample_split]]$ref,
                         mig = sample_splits[[sample_split]]$mig)
  
  # 2) Complete Cases -------------------
  complete_cases <- data_to_complete(train = Lframe, test = Rframe_subset, outcome = outcome_variable)
  Lframe <- complete_cases$train
  Rframe_subset <- complete_cases$test
  
  # 3) Modelling ----------------------
  LRtoOMout <- func_LR_to_OM_boostedtreesCV_binary(outcome = outcome_variable,
                                                   Lframe = Lframe, Rframe = Rframe_subset,
                                                   aid = "aid", rid = "rid",
                                                   cid = "cid", csize = "csize",
                                                   incl.locs = incl.locs, 
                                                   predictors = predictors,
                                                   depth.vec = c(4,5,6),n.trees=1000,
                                                   shrink = 0.01)  
  
  # 4) Mapping ----------------------
  OM <- LRtoOMout[[1]]
  lastvar <- 6
  O <- OM[,1:lastvar]
  M <- as.matrix(OM[,-(1:lastvar)])
  rownames(M) <- O$rid
  Mstar <- func_M_to_Mstar(mmat = M, cid = O$cid, tfunc = compute_femp_prob)
  
  
  # 5) Matching ---------------------
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

  
  # 5) Matching results ----------------------
  
  # Calculate mean outcome
  mean_outcome <- mean(A$outcome)
  
  # Calculate mean predicted probability
  mean_pred <- mean(A$predprob)
  
  # Calculate relative difference
  rel_difference <- (mean_pred - mean_outcome) / mean_outcome
  
  # Number of observations
  n <- nrow(Rframe_subset)
  
  # Number of refugees
  refugees <- sum(Rframe_subset$refugee_sample ==1)
  
  # Assign results to the table
  results <- rbind(results, c(sample_splits[[sample_split]]$name,
                              outcome_variable,
                              mean_outcome,
                              mean_pred,
                              rel_difference,
                              n,
                              refugees))
  
  
  
  # 6) Save results --------------
  saveRDS(LRtoOMout, file.path(paste0(path_data_final,"/",sample_splits[[sample_split]]$name,"/", outcome_variable, "/"),"LRtoOMout.rds"))
  save(A, file = paste0(path_data_final,"/",sample_splits[[sample_split]]$name,"/", outcome_variable, "/", "A.RData"))
  
  
  }
}


# Save overall table
results <- results[-1, ]
save(results, file = paste0(path_data_final,"/","results.RData"))



