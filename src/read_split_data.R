######################### Read Split Data #####################################

read_split_data <- function(split) {
  
  if (split == "refmig_refmig") {
    
    load("data/train15_test16/refmig_refmig/Lframe.RData")
    load("data/train15_test16/refmig_refmig/Rframe.RData")
    
  } else if (split == "refmig_ref") {
    
    load("data/train15_test16/refmig_ref/Lframe.RData")
    load("data/train15_test16/refmig_ref/Rframe.RData")
    
  } else {
    
    stop("Invalid split! Please provide either refmig_refmig or refmig_ref")
  }
  return(list(Lframe = Lframe, Rframe = Rframe))
}
