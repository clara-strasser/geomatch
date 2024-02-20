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


#This function reformats case-level assignment dataframe into
#individual-level assignment dataframe.


# Function arguments and output -------------------------------------------

#Function arguments:
# Astar: case-level assignment dataframe
# O: individual information dataframe
# M: individual-level predicted probability matrix
# print: indicator for printing progress or not (0 no, > 0 yes)

#Function output is individual-level assignment dataframe


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
