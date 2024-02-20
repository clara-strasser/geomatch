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


#This function takes the optmatch() output and reformats it
#into a usable dataframe indicating case assignments.


# Function arguments and output -------------------------------------------

#Function arguments:
# optout: output from pairmatch in optmatch
# aids: vectors of all unique affiliate names
# print: indicator for printing progress or not (0 no, > 0 yes)

#Function output is a dataframe with case ids and location assignments


# Function ----------------------------------------------------------------

func_optmatch_to_Astar <- function(optout,aids,print=0){
  
  optout <- optout[!is.na(optout)]
  l <- length(optout)
  cases <- optout[1:(l/2)]
  cids <- names(cases)
  allocs <- optout[-(1:(l/2))]
  
  Astar.out <- data.frame(cid = cids)
  Astar.out$raid <- NA
  
  for (i in 1:nrow(Astar.out)){
    
    tcid <- Astar.out$cid[i]
    cval <- cases[which(as.numeric(names(cases)) == tcid)]
    Astar.out$raid[i] <- names(allocs)[which(allocs == cval)]
    
    #Progress message
    if(i %% 100==0 & print>0) {
      cat(paste0("iteration: ", i, "\n"))
    }
    
  }
  
  for (i in 1:length(aids)){
    
    taid <- aids[i]
    Astar.out$raid[grep(taid,Astar.out$raid)] <- taid
    
  }
  
  return(Astar.out)
  
}
