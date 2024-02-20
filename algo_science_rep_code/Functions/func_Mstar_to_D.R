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


#This function takes a matrix of predicted probabilities 
#and maps it to a cost/distance matrix (1-pred probabilities) such that 
#every location appears as many times as there are slots.


# Function arguments and output -------------------------------------------

#Function arguments:
# Mstar: case-level predicted probability matrix
# slots: named vector of location capacity slots
# cushion: proportion that locations can accept more cases than specified

#Function output is a square distance matrix (if cushion = 1)
#or wide distance matrix (if cushion > 1) that can be
#used for optimal matching by optmatch()



# Function ----------------------------------------------------------------

func_Mstar_to_D <- function(Mstar,slots,cushion=1){
  
  L <- length(slots)
  D <- 1-replicate(round(cushion*slots[1]),Mstar[,names(slots[1])])
  colnames(D) <- paste(names(slots[1]),1:round(cushion*slots[1]),sep="")
  
  for(i in 2:L){
    cat("location",i,"\n")
    Temp <- 1-replicate(round(cushion*slots[i]),Mstar[,names(slots[i])])
    colnames(Temp) <- paste(names(slots[i]),
                            1:round(cushion*slots[i]),sep="")
    D <- cbind(D,Temp)
  }
  return(D)
  
}
