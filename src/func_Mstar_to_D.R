############################ COST DISTANCE #####################################
# Summary: 
# This function takes a matrix of predicted probabilities 
# and maps it to a cost/distance matrix (1-pred probabilities) such that 
# every location appears as many times as there are slots.


# Arguments --------------------------------------------------------------------

# Mstar: case-level predicted probability matrix
# slots: named vector of location capacity slots
# cushion: proportion that locations can accept more cases than specified

# Output -----------------------------------------------------------------------

#Function output is a square distance matrix (if cushion = 1)
#or wide distance matrix (if cushion > 1) that can be
#used for optimal matching by optmatch()



# Function ---------------------------------------------------------------------

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
