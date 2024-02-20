################################## EMPLOYMENT TO CASE ##########################
# Summary:
# This function maps employment probabilities for individuals within a case
# to a case level employment metric, where the specific metric is chosen
# by the user by specifying the specific mapping function.

# Arguments --------------------------------------------------------------------

# mmat: An individual level predicted probability dataframe (M matrix),
# cid: A vector of case id numbers (ordered to correspond to rows of M)
# tfunc: A mapping function, which will 
#   take the within-case, within-location individual-level probabilities
#   and map those to a single within-location case-level employment metric
#   (e.g. probability that at least 1 refugee in case will be employed,
#   the mapping function for which (compute_femp_prob) is provided below)

# Output -----------------------------------------------------------------------

#Function output is a case-level predicted probability matrix


# Function ---------------------------------------------------------------------

func_M_to_Mstar <- function(mmat,cid,tfunc,print=0){
  
  unique.cids <- sort(unique(cid))
  
  #the output matrix
  mstar <- matrix(NA,nrow = length(unique.cids), ncol = ncol(mmat))
  
  colnames(mstar) <- colnames(mmat)
  rownames(mstar) <- unique.cids
  L <- ncol(mmat)
  
  for (i in 1:length(unique.cids)){
    
    #subsetting to individual case
    caseid <- unique.cids[i]
    tmat <- matrix(mmat[(cid == caseid),],ncol=L)
    
    #performing the mapping across all locations
    mstar[i,] <- apply(tmat, MARGIN = 2, tfunc)
    
    #progress message
    if(i %% 100==0 & print>0) {
      cat(paste0("iteration: ", i, "\n"))
    }
    
  }
  
  return(mstar)
  
}


# Mapping metric ---------------------------------------------------------------

compute_femp_prob <- function(x){
  
  return(1-(prod(1-x)))
  
}
