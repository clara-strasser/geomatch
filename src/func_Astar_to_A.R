############################## CASE TO INDIVIDUAL ##############################
# Summary:
# This function re-formats case-level assignment data frame into
# individual-level assignment data frame.


# Arguments --------------------------------------------------------------------

# Astar: case-level assignment dataframe
# O: individual information dataframe
# M: individual-level predicted probability matrix
# print: indicator for printing progress or not (0 no, > 0 yes)

# Function ----------------------------------------------------------------

func_Astar_to_A <- function(Astar,O,M,print=0){
  
  A <- merge(O,Astar,by = "cid")
  
  A$predprob <- NA
  
  for (i in 1:nrow(A)){
    
    trid <- A$rid[i]
    A$predprob[i] <- M[which(as.numeric(rownames(M)) == trid),
                       which(colnames(M) == A$raid[i])]
    
    #Progress message
    if(i %% 100==0 & print>0) {
      cat(paste0("iteration: ", i, "\n"))
    }
    
  }
  
  return(A)
  
}
