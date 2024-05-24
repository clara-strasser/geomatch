############################## Test Splitting ##################################

rm(list=ls())


# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser"
path_data_soep <- file.path(base_path, "soep_data", "final")
path_data_geomatch <- file.path(base_path, "geomatch_data", "final")
path_data_final <- file.path(base_path, "geomatch_data")


# Read functions ---------------------------------------------------------------
source("src/func_train_test_split.R")
source("src/func_sample_split.R")
source("src/func_data_to_complete.R")

# Read data --------------------------------------------------------------------

# Define path
dir <- paste0(path_data_geomatch, "/data_geomatch.RData")

# Split into Lframe and Rframe
data_splits <- train_test_split(dir, train_year = 2015, test_year = 2016)

# Rows: 9.558
Lframe <- data_splits$train
# Rows: 3.319
Rframe <- data_splits$test


# Specify splits and outcome ---------------------------------------------------

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
                       "employment_three_year_arrival")


# Specify results table --------------------------------------------------------

results <- data.frame(
  sample_split = character(1),
  outcome_variable = character(1),
  n = numeric(1),
  refugees = numeric(1)
)

# Split ------------------------------------------------------------------------

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
    
    # 3) Save results ---------------------
    
    # Number of observations
    n <- nrow(Rframe_subset)
    
    # Number of refugees
    refugees <- sum(Rframe_subset$refugee_sample ==1)
    
    # Save
    results <- rbind(results, c(sample_splits[[sample_split]]$name,
                                outcome_variable,
                                n,
                                refugees))
    


  }
}



