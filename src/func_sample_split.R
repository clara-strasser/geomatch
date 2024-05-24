############################### Sample Split ###################################
# Summary:
# This function filters the test data set to contain only observations for samples
# of interest
# Input:
# data = test data set that has to be filtered
# ref = TRUE, if test data set should contain observations for refugees, else FALSE
# mig = TRUE, if test data set should contain observations for migrants, else FALSE
# Output:
# data = test data set containing only observations for samples of interest


test_sample_split <- function(data, ref=T, mig=T){
  if (ref & mig){
    #Nothing
  }
  else if(ref){
    data <- data %>%
      subset(refugee_sample == 1)
  }
  else{
    data <- data %>%
      subset(refugee_sample == 0)
  }
  return(data)
}


