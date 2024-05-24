############################## Train - Test Split ##############################
# Summary:
# This function splits the data created from IAB-BAMF-SOEP in training and test
# data 
# Input:
# dir = Directory with data set that has to be splitted
# train_year = Max. year included in train data (<= train_year)
# test_year = Min. year included in test data (>=test_year)
# Output:
# For replication purposes:
# Lframe = train data
# Rframe = test data



train_test_split <- function(dir, train_year, test_year) {
  
  ## 1) Load data -------------------------
  load(dir)
  
  ## 2) Split data -------------------------

  # Lframe
  Lframe <- data %>%
    filter(immiyear <= train_year)
  
  # Rframe
  Rframe <- data %>%
    filter(immiyear >= test_year)
  
  ## 3) Handle cases -----------------------
  # Keep only cases that are both in Rframe
  # If a family member of a case is already in Germany
  # than the person in Rframe is assigned to that location.
  # Remove that person from Rframe.
  
  # ID
  Rframe <- Rframe %>%
    mutate(parid_in_id = ifelse(free_case == 0 & !parid %in% rid & same_state == 1, 1,0))
  
  # Keep only that cases
  Rframe <- Rframe %>%
    filter(!parid_in_id == 1)
  
  return(list(train = Lframe, test = Rframe))
}






