############################# Complete Cases data set ##########################
# Summary:
# This function keeps only non-missing values in outcome for train and test data
# Input:
# train: train data set
# test: test data set
# outcome: outcome variable of both data sets, whose NAs should be removed
# Output:
# For replication purposes:
# Lframe = train data
# Rframe = test data


data_to_complete <- function(train, test, outcome){
  outcome_sym <- sym(outcome)
  
  Lframe <- train %>%
    filter(!is.na(!!outcome_sym))
  
  Rframe <- test %>%
    filter(!is.na(!!outcome_sym))
  
  return(list(train = Lframe, test = Rframe))
}
